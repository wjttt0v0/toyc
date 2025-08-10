open Ast

(* 寄存器别名 *)
let sp = "sp"
let fp = "s0"
let ra = "ra"
let a0 = "a0"
let t0 = "t0"
let t1 = "t1"

(* 唯一标签生成器 *)
let label_counter = ref 0
let new_label prefix =
  incr label_counter;
  prefix ^ string_of_int !label_counter

(* --- 优化 1: 常量折叠 (保持) --- *)
let rec eval_const_expr expr =
  match expr with
  | EInt n -> Some n
  | EUnOp (op, e) ->
      (match eval_const_expr e with
      | Some v ->
          (match op with
          | Neg -> Some (-v)
          | Not -> Some (if v = 0 then 1 else 0))
      | None -> None)
  | EBinOp (op, e1, e2) ->
      (match (eval_const_expr e1, eval_const_expr e2) with
      | (Some v1, Some v2) ->
          (try
            Some (match op with
            | Add -> v1 + v2 | Sub -> v1 - v2 | Mul -> v1 * v2
            | Div -> if v2 = 0 then raise Exit else v1 / v2
            | Mod -> if v2 = 0 then raise Exit else v1 mod v2
            | Eq  -> if v1 = v2 then 1 else 0 | Neq -> if v1 <> v2 then 1 else 0
            | Lt  -> if v1 < v2 then 1 else 0 | Le  -> if v1 <= v2 then 1 else 0
            | Gt  -> if v1 > v2 then 1 else 0 | Ge  -> if v1 >= v2 then 1 else 0
            | And -> if v1 <> 0 && v2 <> 0 then 1 else 0
            | Or  -> if v1 <> 0 || v2 <> 0 then 1 else 0)
          with Exit -> None)
      | _ -> None)
  | _ -> None

let is_power_of_two n = n > 0 && (n land (n - 1) = 0)
let log2 n =
  let rec find_log i = if 1 lsl i = n then i else find_log (i + 1) in
  find_log 0

(* 代码生成环境 *)
type env = {
  var_map: (id * int) list;
  current_offset: int;
  exit_label: string;
  break_label: string option;
  continue_label: string option;
}

let find_var_offset id env = try List.assoc id env.var_map with Not_found -> failwith ("Undeclared variable " ^ id)

let rec count_local_vars_stmt stmt =
  match stmt with
  | SDecl _ -> 1 | SBlock stmts -> List.fold_left (fun acc s -> acc + count_local_vars_stmt s) 0 stmts
  | SIf (_, s1, s2_opt) -> count_local_vars_stmt s1 + (match s2_opt with Some s -> count_local_vars_stmt s | None -> 0)
  | SWhile (_, body) -> count_local_vars_stmt body | _ -> 0

(* 代码生成函数现在返回指令列表 (string list) *)
let rec gen_expr env expr : string list =
  match eval_const_expr expr with
  | Some n -> [Printf.sprintf "  li %s, %d" t0 n]
  | None ->
      match expr with
      | EInt n -> [Printf.sprintf "  li %s, %d" t0 n]
      | EId id -> let offset = find_var_offset id env in [Printf.sprintf "  lw %s, %d(%s)" t0 offset fp]
      | EUnOp (op, e) ->
          let e_code = gen_expr env e in
          e_code @ (match op with
          | Neg -> [Printf.sprintf "  neg %s, %s" t0 t0]
          | Not -> [Printf.sprintf "  seqz %s, %s" t0 t0])
      | EBinOp (op, e1, e2) ->
          (match op with
          | And ->
              let false_label = new_label "L_and_false" in
              let end_label = new_label "L_and_end" in
              (gen_expr env e1)
              @ [Printf.sprintf "  beqz %s, %s" t0 false_label]
              @ (gen_expr env e2)
              @ [Printf.sprintf "  snez %s, %s" t0 t0;
                 Printf.sprintf "  j %s" end_label;
                 Printf.sprintf "%s:" false_label;
                 Printf.sprintf "  li %s, 0" t0;
                 Printf.sprintf "%s:" end_label]
          | Or ->
              let true_label = new_label "L_or_true" in
              let end_label = new_label "L_or_end" in
              (gen_expr env e1)
              @ [Printf.sprintf "  bnez %s, %s" t0 true_label]
              @ (gen_expr env e2)
              @ [Printf.sprintf "  snez %s, %s" t0 t0;
                 Printf.sprintf "  j %s" end_label;
                 Printf.sprintf "%s:" true_label;
                 Printf.sprintf "  li %s, 1" t0;
                 Printf.sprintf "%s:" end_label]
          | _ ->
            let e1_const = eval_const_expr e1 in
            let e2_const = eval_const_expr e2 in
            (match (op, e1, e2, e1_const, e2_const) with
            | Add, _, _, Some 0, _ -> gen_expr env e2
            | Add, _, _, _, Some 0 -> gen_expr env e1
            | Sub, _, _, _, Some 0 -> gen_expr env e1
            | Sub, e1, e2, _, _ when e1 = e2 -> [Printf.sprintf "  li %s, 0" t0]
            | Mul, _, _, Some 1, _ -> gen_expr env e2
            | Mul, _, _, _, Some 1 -> gen_expr env e1
            | Mul, _, _, Some 0, _ | Mul, _, _, _, Some 0 -> [Printf.sprintf "  li %s, 0" t0]
            | Div, _, _, _, Some 1 -> gen_expr env e1
            | Mul, _, _, _, Some n when is_power_of_two n -> (gen_expr env e1) @ [Printf.sprintf "  slli %s, %s, %d" t0 t0 (log2 n)]
            | Div, _, _, _, Some n when is_power_of_two n -> (gen_expr env e1) @ [Printf.sprintf "  srai %s, %s, %d" t0 t0 (log2 n)]
            | _ ->
                let e1_code = gen_expr env e1 in
                let e2_code = gen_expr env e2 in
                e1_code
                @ [Printf.sprintf "  addi %s, %s, -4" sp sp; Printf.sprintf "  sw %s, 0(%s)" t0 sp]
                @ e2_code
                @ [Printf.sprintf "  lw %s, 0(%s)" t1 sp; Printf.sprintf "  addi %s, %s, 4" sp sp]
                @ (let r1 = t0 and r2 = t1 in match op with
                  | Add -> [Printf.sprintf "  add %s, %s, %s" r1 r2 r1]
                  | Sub -> [Printf.sprintf "  sub %s, %s, %s" r1 r2 r1]
                  | Mul -> [Printf.sprintf "  mul %s, %s, %s" r1 r2 r1]
                  | Div -> [Printf.sprintf "  div %s, %s, %s" r1 r2 r1]
                  | Mod -> [Printf.sprintf "  rem %s, %s, %s" r1 r2 r1]
                  | Eq  -> [Printf.sprintf "  sub %s, %s, %s; seqz %s, %s" r1 r2 r1 r1 r1]
                  | Neq -> [Printf.sprintf "  sub %s, %s, %s; snez %s, %s" r1 r2 r1 r1 r1]
                  | Lt  -> [Printf.sprintf "  slt %s, %s, %s" r1 r2 r1]
                  | Gt  -> [Printf.sprintf "  sgt %s, %s, %s" r1 r2 r1]
                  (*** START OF FIXED SECTION ***)
                  | Le  -> [Printf.sprintf "  sgt %s, %s, %s; xori %s, %s, 1" r1 r1 r2 r1 r1]
                  | Ge  -> [Printf.sprintf "  slt %s, %s, %s; xori %s, %s, 1" r1 r1 r2 r1 r1]
                  (*** END OF FIXED SECTION ***)
                  | _ -> [])
            )
          )
      | ECall (id, args) ->
          let arg_space = 4 * List.length args in
          let reversed_args = List.rev args in
          let arg_eval_code = List.fold_left (fun acc_code arg ->
              let current_arg_code = gen_expr env arg in
              let push_code = [
                  Printf.sprintf "  addi %s, %s, -4" sp sp;
                  Printf.sprintf "  sw %s, 0(%s)" t0 sp
                ] in
              acc_code @ current_arg_code @ push_code
            ) [] reversed_args in
          let call_instruction = [Printf.sprintf "  call %s" id] in
          let cleanup_code = if arg_space > 0 then [Printf.sprintf "  addi %s, %s, %d" sp sp arg_space] else [] in
          let move_return_value = [Printf.sprintf "  mv %s, %s" t0 a0] in
          arg_eval_code @ call_instruction @ cleanup_code @ move_return_value

and gen_stmt env stmt : string list =
  match stmt with
  | SExpr e -> gen_expr env e
  | SReturn (Some e) -> (gen_expr env e) @ [Printf.sprintf "  mv %s, %s" a0 t0; Printf.sprintf "  j %s" env.exit_label]
  | SReturn None -> [Printf.sprintf "  j %s" env.exit_label]
  | SDecl (id, e) -> let offset = find_var_offset id env in (gen_expr env e) @ [Printf.sprintf "  sw %s, %d(%s)" t0 offset fp]
  | SAssign (id, e) -> let offset = find_var_offset id env in (gen_expr env e) @ [Printf.sprintf "  sw %s, %d(%s)" t0 offset fp]
  | SIf (cond, then_stmt, else_opt) ->
      let else_label = new_label "L_else" in
      let end_label = new_label "L_if_end" in
      let cond_code = gen_expr env cond in
      (match else_opt with
      | Some else_stmt ->
          cond_code @ [Printf.sprintf "  beqz %s, %s" t0 else_label]
          @ (gen_stmt env then_stmt) @ [Printf.sprintf "  j %s" end_label]
          @ [Printf.sprintf "%s:" else_label] @ (gen_stmt env else_stmt)
          @ [Printf.sprintf "%s:" end_label]
      | None ->
          cond_code @ [Printf.sprintf "  beqz %s, %s" t0 end_label]
          @ (gen_stmt env then_stmt) @ [Printf.sprintf "%s:" end_label]
      )
  | SWhile (cond, body) ->
      let start_label = new_label "L_while_start" in
      let end_label = new_label "L_while_end" in
      let loop_env = { env with break_label = Some end_label; continue_label = Some start_label } in
      [Printf.sprintf "%s:" start_label]
      @ (gen_expr env cond) @ [Printf.sprintf "  beqz %s, %s" t0 end_label]
      @ (gen_stmt loop_env body) @ [Printf.sprintf "  j %s" start_label]
      @ [Printf.sprintf "%s:" end_label]
  | SBreak -> (match env.break_label with Some l -> [Printf.sprintf "  j %s" l] | None -> failwith "break")
  | SContinue -> (match env.continue_label with Some l -> [Printf.sprintf "  j %s" l] | None -> failwith "continue")
  | SBlock stmts ->
      let locals_in_block = count_local_vars_stmt (SBlock stmts) in
      let block_space = locals_in_block * 4 in
      let block_env = 
        let current_offset = (match env.var_map with | (_, off)::_ -> off-4 | [] -> -8) in
        let (updated_var_map, _) = List.fold_left (fun (map, off) s ->
          match s with
          | SDecl(id, _) -> ((id, off) :: map, off-4)
          | _ -> (map, off)
        ) (env.var_map, current_offset) stmts in
        { env with var_map = updated_var_map } in
      (if block_space > 0 then [Printf.sprintf "  addi %s, %s, -%d" sp sp block_space] else [])
      @ (List.concat_map (gen_stmt block_env) stmts)
      @ (if block_space > 0 then [Printf.sprintf "  addi %s, %s, %d" sp sp block_space] else [])

let gen_func_def f : string list =
  let exit_label = new_label ("L_exit_" ^ f.name) in
  let num_locals = count_local_vars_stmt f.body in
  let locals_space = num_locals * 4 in
  let env_with_params, _ = List.fold_left (fun (env, i) (PInt id) ->
    let param_offset = 8 + i * 4 in
    ({env with var_map = (id, param_offset) :: env.var_map}, i + 1)
  ) (({ var_map = []; current_offset = -8; exit_label; break_label=None; continue_label=None }), 0) f.params in
  
  let rec build_env_for_locals env stmt_list =
    List.fold_left (fun e s ->
        match s with
        | SDecl(id, _) -> {e with var_map = (id, e.current_offset)::e.var_map; current_offset = e.current_offset - 4}
        | SBlock stmts -> build_env_for_locals e stmts
        | SIf (_, s1, Some s2) -> build_env_for_locals (build_env_for_locals e [s1]) [s2]
        | SIf (_, s1, None) -> build_env_for_locals e [s1]
        | SWhile (_, s) -> build_env_for_locals e [s]
        | _ -> e
    ) env stmt_list
  in
  let final_env = build_env_for_locals env_with_params (match f.body with SBlock b -> b | _ -> [f.body]) in
  let body_code = gen_stmt final_env f.body in
  
  [Printf.sprintf ".globl %s" f.name; Printf.sprintf "%s:" f.name]
  @ [Printf.sprintf "  addi %s, %s, -8" sp sp; Printf.sprintf "  sw %s, 4(%s)" ra sp; Printf.sprintf "  sw %s, 0(%s)" fp sp; Printf.sprintf "  mv %s, %s" fp sp]
  @ (if locals_space > 0 then [Printf.sprintf "  addi %s, %s, -%d" sp sp locals_space] else [])
  @ body_code
  @ [Printf.sprintf "%s:" exit_label]
  @ [Printf.sprintf "  mv %s, %s" sp fp; Printf.sprintf "  lw %s, 4(%s)" ra sp; Printf.sprintf "  lw %s, 0(%s)" fp sp; Printf.sprintf "  addi %s, %s, 8" sp sp; Printf.sprintf "  ret\n"]

let peephole_optimize (code: string list) : string list =
  let rec optimize_pass instrs =
    match instrs with
    | ins1 :: ins2 :: rest when (Scanf.sscanf_opt ins1 "  sw %s, %d(%s@)" (fun r1 n1 b1 -> Scanf.sscanf_opt ins2 "  lw %s, %d(%s@)" (fun r2 n2 b2 -> if r1=r2 && n1=n2 && b1=b2 then Some () else None)) |> Option.is_some) -> ins1 :: (optimize_pass rest)
    | ins1 :: rest when (String.starts_with ~prefix:"  j" ins1 || String.starts_with ~prefix:"  ret" ins1) -> let rec drop_unreachable remaining = match remaining with | [] -> [] | l :: ls when String.ends_with ~suffix:":" l -> l :: ls | _ :: ls -> drop_unreachable ls in ins1 :: (optimize_pass (drop_unreachable rest))
    | ins :: rest -> ins :: (optimize_pass rest)
    | [] -> []
  in
  let prev_code_ref = ref [] in
  let next_code_ref = ref code in
  while !next_code_ref <> !prev_code_ref do prev_code_ref := !next_code_ref; next_code_ref := optimize_pass !next_code_ref done;
  !next_code_ref

let gen_comp_unit oc (CUnit funcs) =
  let unoptimized_code = [".text\n.global main"] @ List.concat_map gen_func_def funcs in
  let optimized_code = peephole_optimize unoptimized_code in
  List.iter (fun line -> Printf.fprintf oc "%s\n" line) optimized_code
