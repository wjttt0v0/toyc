open Ast

(* 寄存器别名 *)
let sp = "sp"
let fp = "s0"
let ra = "ra"
let a0 = "a0"
let t0 = "t0"
let t1 = "t1"
let t2 = "t2"
let t3 = "t3"

(* 唯一标签生成器 *)
let label_counter = ref 0
let new_label prefix =
  incr label_counter;
  prefix ^ string_of_int !label_counter

(* --- 优化 1: 常量折叠 --- *)
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

let find_var_offset id env = 
  try List.assoc id env.var_map 
  with Not_found -> failwith ("Undeclared variable " ^ id)

let rec count_local_vars_stmt stmt =
  match stmt with
  | SDecl _ -> 1 
  | SBlock stmts -> List.fold_left (fun acc s -> acc + count_local_vars_stmt s) 0 stmts
  | SIf (_, s1, s2_opt) -> count_local_vars_stmt s1 + 
      (match s2_opt with Some s -> count_local_vars_stmt s | None -> 0)
  | SWhile (_, body) -> count_local_vars_stmt body 
  | _ -> 0

(* --- 优化 2: 改进的寄存器分配和临时存储管理 --- *)
type temp_reg = 
  | T0 | T1 | T2 | T3 | T4 | T5

let string_of_temp_reg = function
  | T0 -> "t0" | T1 -> "t1" | T2 -> "t2" 
  | T3 -> "t3" | T4 -> "t4" | T5 -> "t5"

let next_temp_reg = function
  | T0 -> T1 | T1 -> T2 | T2 -> T3 | T3 -> T4 | T4 -> T5 | T5 -> T0

(* --- 优化 3: 智能栈管理 --- *)
let save_caller_regs used_regs =
  let all_caller_regs = ["t0"; "t1"; "t2"; "t3"; "t4"; "t5"; "a0"] in
  let regs_to_save = List.filter (fun r -> List.mem r used_regs) all_caller_regs in
  let save_size = List.length regs_to_save * 4 in
  let save_instrs = 
    if save_size > 0 then
      [Printf.sprintf "  addi %s, %s, -%d" sp sp save_size] @
      (List.mapi (fun i reg -> 
        Printf.sprintf "  sw %s, %d(%s)" reg (i * 4) sp
      ) regs_to_save)
    else []
  in
  let restore_instrs =
    if save_size > 0 then
      (List.mapi (fun i reg -> 
        Printf.sprintf "  lw %s, %d(%s)" reg (i * 4) sp
      ) regs_to_save) @
      [Printf.sprintf "  addi %s, %s, %d" sp sp save_size]
    else []
  in
  (save_instrs, restore_instrs)

(* 代码生成函数，返回 (指令列表, 使用的寄存器列表) *)
let rec gen_expr_with_regs env expr target_reg : string list * string list =
  match eval_const_expr expr with
  | Some n -> ([Printf.sprintf "  li %s, %d" target_reg n], [target_reg])
  | None ->
      match expr with
      | EInt n -> ([Printf.sprintf "  li %s, %d" target_reg n], [target_reg])
      | EId id -> 
          let offset = find_var_offset id env in 
          ([Printf.sprintf "  lw %s, %d(%s)" target_reg offset fp], [target_reg])
      | EUnOp (op, e) ->
          let (e_code, e_regs) = gen_expr_with_regs env e target_reg in
          let op_code = match op with
            | Neg -> [Printf.sprintf "  neg %s, %s" target_reg target_reg]
            | Not -> [Printf.sprintf "  seqz %s, %s" target_reg target_reg]
          in
          (e_code @ op_code, e_regs)
      | EBinOp (op, e1, e2) ->
          (match op with
          | And ->
              let false_label = new_label "L_and_false" in
              let end_label = new_label "L_and_end" in
              let (e1_code, e1_regs) = gen_expr_with_regs env e1 target_reg in
              let (e2_code, e2_regs) = gen_expr_with_regs env e2 target_reg in
              (e1_code
              @ [Printf.sprintf "  beqz %s, %s" target_reg false_label]
              @ e2_code
              @ [Printf.sprintf "  snez %s, %s" target_reg target_reg;
                 Printf.sprintf "  j %s" end_label;
                 Printf.sprintf "%s:" false_label;
                 Printf.sprintf "  li %s, 0" target_reg;
                 Printf.sprintf "%s:" end_label], e1_regs @ e2_regs)
          | Or ->
              let true_label = new_label "L_or_true" in
              let end_label = new_label "L_or_end" in
              let (e1_code, e1_regs) = gen_expr_with_regs env e1 target_reg in
              let (e2_code, e2_regs) = gen_expr_with_regs env e2 target_reg in
              (e1_code
              @ [Printf.sprintf "  bnez %s, %s" target_reg true_label]
              @ e2_code
              @ [Printf.sprintf "  snez %s, %s" target_reg target_reg;
                 Printf.sprintf "  j %s" end_label;
                 Printf.sprintf "%s:" true_label;
                 Printf.sprintf "  li %s, 1" target_reg;
                 Printf.sprintf "%s:" end_label], e1_regs @ e2_regs)
          | _ ->
              (* 代数化简 & 强度削减 *)
              let e1_const = eval_const_expr e1 in
              let e2_const = eval_const_expr e2 in
              (match (op, e1, e2, e1_const, e2_const) with
              | Add, _, _, Some 0, _ -> gen_expr_with_regs env e2 target_reg
              | Add, _, _, _, Some 0 -> gen_expr_with_regs env e1 target_reg
              | Sub, _, _, _, Some 0 -> gen_expr_with_regs env e1 target_reg
              | Sub, e1, e2, _, _ when e1 = e2 -> ([Printf.sprintf "  li %s, 0" target_reg], [target_reg])
              | Mul, _, _, Some 1, _ -> gen_expr_with_regs env e2 target_reg
              | Mul, _, _, _, Some 1 -> gen_expr_with_regs env e1 target_reg
              | Mul, _, _, Some 0, _ | Mul, _, _, _, Some 0 -> ([Printf.sprintf "  li %s, 0" target_reg], [target_reg])
              | Div, _, _, _, Some 1 -> gen_expr_with_regs env e1 target_reg
              | Mul, _, _, _, Some n when is_power_of_two n -> 
                  let (e1_code, e1_regs) = gen_expr_with_regs env e1 target_reg in
                  (e1_code @ [Printf.sprintf "  slli %s, %s, %d" target_reg target_reg (log2 n)], e1_regs)
              | Div, _, _, _, Some n when is_power_of_two n -> 
                  let (e1_code, e1_regs) = gen_expr_with_regs env e1 target_reg in
                  (e1_code @ [Printf.sprintf "  srai %s, %s, %d" target_reg target_reg (log2 n)], e1_regs)
              | _ ->
                  (* 使用两个不同的寄存器 *)
                  let aux_reg = "t1" in
                  let (e1_code, e1_regs) = gen_expr_with_regs env e1 target_reg in
                  let (e2_code, e2_regs) = gen_expr_with_regs env e2 aux_reg in
                  let op_code = match op with
                    | Add -> [Printf.sprintf "  add %s, %s, %s" target_reg target_reg aux_reg]
                    | Sub -> [Printf.sprintf "  sub %s, %s, %s" target_reg target_reg aux_reg]
                    | Mul -> [Printf.sprintf "  mul %s, %s, %s" target_reg target_reg aux_reg]
                    | Div -> [Printf.sprintf "  div %s, %s, %s" target_reg target_reg aux_reg]
                    | Mod -> [Printf.sprintf "  rem %s, %s, %s" target_reg target_reg aux_reg]
                    | Eq  -> [Printf.sprintf "  sub %s, %s, %s" target_reg target_reg aux_reg; 
                             Printf.sprintf "  seqz %s, %s" target_reg target_reg]
                    | Neq -> [Printf.sprintf "  sub %s, %s, %s" target_reg target_reg aux_reg; 
                             Printf.sprintf "  snez %s, %s" target_reg target_reg]
                    | Lt  -> [Printf.sprintf "  slt %s, %s, %s" target_reg target_reg aux_reg]
                    | Le  -> [Printf.sprintf "  sgt %s, %s, %s" target_reg target_reg aux_reg; 
                             Printf.sprintf "  xori %s, %s, 1" target_reg target_reg]
                    | Gt  -> [Printf.sprintf "  sgt %s, %s, %s" target_reg target_reg aux_reg]
                    | Ge  -> [Printf.sprintf "  slt %s, %s, %s" target_reg target_reg aux_reg; 
                             Printf.sprintf "  xori %s, %s, 1" target_reg target_reg]
                    | _ -> []
                  in
                  (e1_code @ e2_code @ op_code, e1_regs @ e2_regs @ [aux_reg])
              )
          )
      | ECall (id, args) ->
          (* 优化函数调用：只保存实际使用的寄存器 *)
          let used_regs = [target_reg; "t1"] in  (* 简化：假设这些是可能被使用的 *)
          let (save_code, restore_code) = save_caller_regs used_regs in
          
          (* 参数求值并压栈 *)
          let reversed_args = List.rev args in
          let arg_eval_code = List.fold_left (fun acc_code arg ->
              let (current_arg_code, _) = gen_expr_with_regs env arg "t0" in
              let push_code = [
                  Printf.sprintf "  addi %s, %s, -4" sp sp;
                  Printf.sprintf "  sw %s, 0(%s)" "t0" sp
                ] in
              acc_code @ current_arg_code @ push_code
            ) [] reversed_args in

          let arg_space = 4 * List.length args in
          let call_code = [Printf.sprintf "  call %s" id] in
          let cleanup_code = 
              if arg_space > 0 then [Printf.sprintf "  addi %s, %s, %d" sp sp arg_space] else [] in
          let move_result = [Printf.sprintf "  mv %s, %s" target_reg a0] in

          (save_code @ arg_eval_code @ call_code @ cleanup_code @ move_result @ restore_code, 
           used_regs @ ["a0"])

(* 简化的表达式生成接口 *)
let gen_expr env expr = 
  let (code, _) = gen_expr_with_regs env expr "t0" in code

let rec gen_stmt env stmt : string list =
  match stmt with
  | SExpr e -> gen_expr env e
  | SReturn (Some e) -> 
      (gen_expr env e) @ 
      [Printf.sprintf "  mv %s, %s" a0 t0; Printf.sprintf "  j %s" env.exit_label]
  | SReturn None -> [Printf.sprintf "  j %s" env.exit_label]
  | SDecl (id, e) -> 
      let offset = find_var_offset id env in 
      (gen_expr env e) @ [Printf.sprintf "  sw %s, %d(%s)" t0 offset fp]
  | SAssign (id, e) -> 
      let offset = find_var_offset id env in 
      (gen_expr env e) @ [Printf.sprintf "  sw %s, %d(%s)" t0 offset fp]
  | SIf (cond, then_stmt, else_opt) ->
      let else_label = new_label "L_else" in
      let end_label = new_label "L_if_end" in
      let cond_code = gen_expr env cond in
      (match else_opt with
      | Some else_stmt ->
          cond_code
          @ [Printf.sprintf "  beqz %s, %s" t0 else_label]
          @ (gen_stmt env then_stmt)
          @ [Printf.sprintf "  j %s" end_label]
          @ [Printf.sprintf "%s:" else_label]
          @ (gen_stmt env else_stmt)
          @ [Printf.sprintf "%s:" end_label]
      | None ->
          cond_code
          @ [Printf.sprintf "  beqz %s, %s" t0 end_label]
          @ (gen_stmt env then_stmt)
          @ [Printf.sprintf "%s:" end_label]
      )
  | SWhile (cond, body) ->
      let start_label = new_label "L_while_start" in
      let end_label = new_label "L_while_end" in
      let loop_env = { env with break_label = Some end_label; continue_label = Some start_label } in
      [Printf.sprintf "%s:" start_label]
      @ (gen_expr env cond)
      @ [Printf.sprintf "  beqz %s, %s" t0 end_label]
      @ (gen_stmt loop_env body)
      @ [Printf.sprintf "  j %s" start_label]
      @ [Printf.sprintf "%s:" end_label]
  | SBreak -> 
      (match env.break_label with 
       | Some l -> [Printf.sprintf "  j %s" l] 
       | None -> failwith "break outside loop")
  | SContinue -> 
      (match env.continue_label with 
       | Some l -> [Printf.sprintf "  j %s" l] 
       | None -> failwith "continue outside loop")
  | SBlock stmts ->
      let locals_in_block = count_local_vars_stmt (SBlock stmts) in
      let block_space = locals_in_block * 4 in
      let block_env = 
        let new_offset = env.current_offset - block_space in
        let (updated_var_map, _) = List.fold_left (fun (map, off) s ->
          match s with
          | SDecl(id, _) -> let new_off = off - 4 in ((id, new_off) :: map, new_off)
          | _ -> (map, off)
        ) (env.var_map, env.current_offset) stmts in
        { env with var_map = updated_var_map; current_offset = new_offset } in
      let body_code = List.concat_map (gen_stmt block_env) stmts in
      (if block_space > 0 then [Printf.sprintf "  addi %s, %s, -%d" sp sp block_space] else [])
      @ body_code
      @ (if block_space > 0 then [Printf.sprintf "  addi %s, %s, %d" sp sp block_space] else [])

let gen_func_def f : string list =
  let exit_label = new_label ("L_exit_" ^ f.name) in
  let num_locals = count_local_vars_stmt f.body in
  let locals_space = num_locals * 4 in
  
  (* 构建参数环境 *)
  let env_with_params, _ = List.fold_left (fun (env, i) (PInt id) ->
    let param_offset = 8 + i * 4 in
    ({env with var_map = (id, param_offset) :: env.var_map}, i + 1)
  ) (({ var_map = []; current_offset = 0; exit_label; break_label=None; continue_label=None }), 0) f.params in
  
  (* 构建局部变量环境 *)
  let rec build_env_for_locals current_offset stmts =
    List.fold_left (fun (env, offset) stmt ->
      match stmt with
      | SDecl (id, _) -> 
          let new_offset = offset - 4 in 
          ({ env with var_map = (id, new_offset) :: env.var_map }, new_offset)
      | SBlock block_stmts -> build_env_for_locals offset block_stmts
      | SIf (_, s1, s2o) ->
          let env', off' = build_env_for_locals offset [s1] in
          (match s2o with Some s2 -> build_env_for_locals off' [s2] | None -> (env', off'))
      | SWhile (_, s) -> build_env_for_locals offset [s] 
      | _ -> (env, offset)
    ) (env_with_params, current_offset) stmts in
    
  let final_env, _ = build_env_for_locals 0 (match f.body with SBlock stmts -> stmts | s -> [s]) in
  let body_code = gen_stmt final_env f.body in
  
  (* 函数前导码 *)
  [Printf.sprintf "%s:" f.name]
  @ [Printf.sprintf "  addi %s, %s, -8" sp sp; 
     Printf.sprintf "  sw %s, 4(%s)" ra sp; 
     Printf.sprintf "  sw %s, 0(%s)" fp sp; 
     Printf.sprintf "  mv %s, %s" fp sp]
  @ (if locals_space > 0 then [Printf.sprintf "  addi %s, %s, -%d" sp sp locals_space] else [])
  @ body_code
  @ [Printf.sprintf "%s:" exit_label]
  @ [Printf.sprintf "  mv %s, %s" sp fp; 
     Printf.sprintf "  lw %s, 4(%s)" ra sp; 
     Printf.sprintf "  lw %s, 0(%s)" fp sp; 
     Printf.sprintf "  addi %s, %s, 8" sp sp; 
     Printf.sprintf "  ret\n"]

(* --- 优化 4: 增强的窥孔优化 --- *)
let peephole_optimize (code: string list) : string list =
  let rec optimize_pass instrs =
    match instrs with
    (* 消除冗余的存取操作 *)
    | ins1 :: ins2 :: rest when (
        Scanf.sscanf_opt ins1 "  sw %s, %d(%s@)" (fun r1 n1 b1 ->
        Scanf.sscanf_opt ins2 "  lw %s, %d(%s@)" (fun r2 n2 b2 ->
            if r1 = r2 && n1 = n2 && b1 = b2 then Some () else None
        )) |> Option.is_some
      ) -> ins1 :: (optimize_pass rest)
    
    (* 优化连续的立即数加载 *)
    | ins1 :: ins2 :: rest when (
        Scanf.sscanf_opt ins1 "  li %s, 0@" (fun r1 ->
        Scanf.sscanf_opt ins2 "  li %s, %d@" (fun r2 n ->
            if r1 = r2 then Some n else None
        )) |> Option.is_some
      ) -> ins2 :: (optimize_pass rest)
    
    (* 移除跳转后的死代码 *)
    | ins1 :: rest when (String.starts_with ~prefix:"  j" ins1 || String.starts_with ~prefix:"  ret" ins1) ->
        let rec drop_unreachable remaining =
          match remaining with
          | [] -> []
          | l :: ls when String.ends_with ~suffix:":" l -> l :: ls
          | _ :: ls -> drop_unreachable ls
        in
        ins1 :: (optimize_pass (drop_unreachable rest))
    
    (* 优化无用的移动指令 *)
    | ins :: rest when (
        Scanf.sscanf_opt ins "  mv %s, %s@" (fun r1 r2 ->
            if r1 = r2 then Some () else None
        ) |> Option.is_some
      ) -> optimize_pass rest
    
    | ins :: rest -> ins :: (optimize_pass rest)
    | [] -> []
  in
  
  (* 多轮优化直到收敛 *)
  let prev_code_ref = ref [] in
  let next_code_ref = ref code in
  while !next_code_ref <> !prev_code_ref do
    prev_code_ref := !next_code_ref;
    next_code_ref := optimize_pass !next_code_ref
  done;
  !next_code_ref

(* 主生成函数 *)
let gen_comp_unit oc (CUnit funcs) =
  let unoptimized_code = [".text\n.global main"] @ List.concat_map gen_func_def funcs in
  let optimized_code = peephole_optimize unoptimized_code in
  List.iter (fun line -> Printf.fprintf oc "%s\n" line) optimized_code
