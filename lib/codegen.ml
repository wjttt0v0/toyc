open Ast

let sp = "sp"
let fp = "s0"
let ra = "ra"
let a0 = "a0"
let all_temp_regs = ["t0"; "t1"; "t2"; "t3"; "t4"; "t5"; "t6"]

let label_counter = ref 0
let new_label prefix =
  incr label_counter;
  prefix ^ string_of_int !label_counter

let rec eval_const_expr expr =
  match expr with
  | EInt n -> Some n | EUnOp (op, e) -> (match eval_const_expr e with Some v -> (match op with Neg -> Some (-v) | Not -> Some (if v = 0 then 1 else 0)) | None -> None)
  | EBinOp (op, e1, e2) -> (match (eval_const_expr e1, eval_const_expr e2) with
    | (Some v1, Some v2) -> (try Some (match op with
      | Add->v1+v2 | Sub->v1-v2 | Mul->v1*v2 | Div->if v2=0 then raise Exit else v1/v2 | Mod->if v2=0 then raise Exit else v1 mod v2
      | Eq->if v1=v2 then 1 else 0 | Neq->if v1<>v2 then 1 else 0 | Lt->if v1<v2 then 1 else 0 | Le->if v1<=v2 then 1 else 0
      | Gt->if v1>v2 then 1 else 0 | Ge->if v1>=v2 then 1 else 0 | And->if v1<>0 && v2<>0 then 1 else 0 | Or->if v1<>0 || v2<>0 then 1 else 0)
      with Exit -> None) | _ -> None) | _ -> None

let is_power_of_two n = n > 0 && (n land (n - 1) = 0)
let log2 n = let rec find_log i = if 1 lsl i = n then i else find_log (i+1) in find_log 0

type env = {
  var_map: (id * int) list;
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

let rec gen_expr env regs expr : string list =
  match eval_const_expr expr with
  | Some n -> [Printf.sprintf "  li %s, %d" (List.hd regs) n]
  | None ->
    let target_reg, rest_regs = match regs with
      | hd :: tl -> hd, tl
      | [] -> failwith "Expression too complex: Ran out of registers. Spilling required."
    in
    match expr with
    | EInt n -> [Printf.sprintf "  li %s, %d" target_reg n]
    | EId id -> let offset = find_var_offset id env in [Printf.sprintf "  lw %s, %d(%s)" target_reg offset fp]
    | EUnOp (op, e) ->
        (gen_expr env regs e) @
        (match op with
         | Neg -> [Printf.sprintf "  neg %s, %s" target_reg target_reg]
         | Not -> [Printf.sprintf "  seqz %s, %s" target_reg target_reg])
    | EBinOp (op, e1, e2) ->
        (match op with
        | And ->
            let false_label = new_label "L_and_false" in let end_label = new_label "L_and_end" in
            (gen_expr env regs e1) @ [Printf.sprintf "  beqz %s, %s" target_reg false_label] @ (gen_expr env regs e2) @
            [Printf.sprintf "  snez %s, %s" target_reg target_reg; Printf.sprintf "  j %s" end_label;
             Printf.sprintf "%s:" false_label; Printf.sprintf "  li %s, 0" target_reg; Printf.sprintf "%s:" end_label]
        | Or ->
            let true_label = new_label "L_or_true" in let end_label = new_label "L_or_end" in
            (gen_expr env regs e1) @ [Printf.sprintf "  bnez %s, %s" target_reg true_label] @ (gen_expr env regs e2) @
            [Printf.sprintf "  snez %s, %s" target_reg target_reg; Printf.sprintf "  j %s" end_label;
            Printf.sprintf "%s:" true_label; Printf.sprintf "  li %s, 1" target_reg; Printf.sprintf "%s:" end_label]
        | _ ->
            (match (op, e1, e2, eval_const_expr e1, eval_const_expr e2) with
             | Add, _, _, Some 0, _ -> gen_expr env regs e2 | Add, _, _, _, Some 0 -> gen_expr env regs e1
             | Sub, _, _, _, Some 0 -> gen_expr env regs e1 | Sub, _, _, Some v, _ when v=0 && e1=e2 -> [Printf.sprintf "  li %s, 0" target_reg]
             | Mul, _, _, Some 1, _ -> gen_expr env regs e2 | Mul, _, _, _, Some 1 -> gen_expr env regs e1
             | Mul, _, _, Some 0, _ | Mul, _, _, _, Some 0 -> [Printf.sprintf "  li %s, 0" target_reg]
             | Div, _, _, _, Some 1 -> gen_expr env regs e1
             | Mul, _, _, _, Some n when is_power_of_two n -> (gen_expr env regs e1) @ [Printf.sprintf "  slli %s, %s, %d" target_reg target_reg (log2 n)]
             | Div, _, _, _, Some n when is_power_of_two n -> (gen_expr env regs e1) @ [Printf.sprintf "  srai %s, %s, %d" target_reg target_reg (log2 n)]
             | _ ->
                 let e1_code = gen_expr env regs e1 in
                 let r1 = target_reg in
                 if rest_regs = [] then
                    let r_spill = List.hd all_temp_regs in let r_other = List.nth all_temp_regs 1 in
                    e1_code @ [Printf.sprintf "  addi %s, %s, -4" sp sp; Printf.sprintf "  sw %s, 0(%s)" r_spill sp]
                    @ (gen_expr env regs e2)
                    @ [Printf.sprintf "  lw %s, 0(%s)" r_other sp; Printf.sprintf "  addi %s, %s, 4" sp sp]
                    @ (match op with
                       | Add->[Printf.sprintf "  add %s,%s,%s" r_spill r_other r_spill] | Sub->[Printf.sprintf "  sub %s,%s,%s" r_spill r_other r_spill]
                       | Mul->[Printf.sprintf "  mul %s,%s,%s" r_spill r_other r_spill] | Div->[Printf.sprintf "  div %s,%s,%s" r_spill r_other r_spill]
                       | Mod->[Printf.sprintf "  rem %s,%s,%s" r_spill r_other r_spill] | Eq ->[Printf.sprintf "  sub %s,%s,%s; seqz %s,%s" r_spill r_other r_spill r_spill r_spill]
                       | Neq->[Printf.sprintf "  sub %s,%s,%s; snez %s,%s" r_spill r_other r_spill r_spill r_spill] | Lt ->[Printf.sprintf "  slt %s,%s,%s" r_spill r_other r_spill]
                       | Gt ->[Printf.sprintf "  sgt %s,%s,%s" r_spill r_other r_spill] | Le ->[Printf.sprintf "  sgt %s,%s,%s; xori %s,%s,1" r_spill r_spill r_other]
                       | Ge ->[Printf.sprintf "  slt %s,%s,%s; xori %s,%s,1" r_spill r_spill r_other])
                 else
                    let e2_code = gen_expr env rest_regs e2 in
                    let r2 = List.hd rest_regs in
                    e1_code @ e2_code @
                    (match op with
                     | Add->[Printf.sprintf "  add %s,%s,%s" r1 r1 r2] | Sub->[Printf.sprintf "  sub %s,%s,%s" r1 r1 r2]
                     | Mul->[Printf.sprintf "  mul %s,%s,%s" r1 r1 r2] | Div->[Printf.sprintf "  div %s,%s,%s" r1 r1 r2]
                     | Mod->[Printf.sprintf "  rem %s,%s,%s" r1 r1 r2] | Eq ->[Printf.sprintf "  sub %s,%s,%s; seqz %s,%s" r1 r1 r2 r1 r1]
                     | Neq->[Printf.sprintf "  sub %s,%s,%s; snez %s,%s" r1 r1 r2 r1 r1] | Lt ->[Printf.sprintf "  slt %s,%s,%s" r1 r1 r2]
                     | Gt ->[Printf.sprintf "  sgt %s,%s,%s" r1 r1 r2] | Le ->[Printf.sprintf "  sgt %s,%s,%s; xori %s,%s,1" r1 r1 r2 r1 r1]
                     | Ge ->[Printf.sprintf "  slt %s,%s,%s; xori %s,%s,1" r1 r1 r2 r1 r1])
            )
        )
    | ECall (id, args) ->
        let num_regs_to_save = List.length all_temp_regs in
        let save_offset = -4 * num_regs_to_save in
        let save_code = (List.mapi (fun i r -> Printf.sprintf "  sw %s, %d(%s)" r (i * -4 - 4) sp) all_temp_regs) in
        let arg_eval_code = List.fold_left (fun acc_code arg ->
            acc_code @ (gen_expr env all_temp_regs arg)
            @ [Printf.sprintf "  addi %s, %s, -4" sp sp; Printf.sprintf "  sw %s, 0(%s)" (List.hd all_temp_regs) sp]
          ) [] (List.rev args) in
        let call_instruction = [Printf.sprintf "  call %s" id] in
        let cleanup_args_stack = if args <> [] then [Printf.sprintf "  addi %s, %s, %d" sp sp (4 * List.length args)] else [] in
        let restore_code = (List.mapi (fun i r -> Printf.sprintf "  lw %s, %d(%s)" r (i * -4 - 4) sp) all_temp_regs) in
        [Printf.sprintf "  addi %s, %s, %d" sp sp save_offset] @ save_code
        @ arg_eval_code @ call_instruction @ cleanup_args_stack
        @ restore_code @ [Printf.sprintf "  addi %s, %s, %d" sp sp (-save_offset)]
        @ [Printf.sprintf "  mv %s, %s" target_reg a0]

and gen_stmt env stmt : string list =
  match stmt with
  | SExpr e -> gen_expr env all_temp_regs e
  | SReturn (Some e) -> gen_expr env all_temp_regs e @ [Printf.sprintf "  mv %s, %s" a0 (List.hd all_temp_regs); Printf.sprintf "  j %s" env.exit_label]
  | SReturn None -> [Printf.sprintf "  j %s" env.exit_label]
  | SDecl (id, e) -> let offset = find_var_offset id env in (gen_expr env all_temp_regs e) @ [Printf.sprintf "  sw %s, %d(%s)" (List.hd all_temp_regs) offset fp]
  | SAssign (id, e) -> let offset = find_var_offset id env in (gen_expr env all_temp_regs e) @ [Printf.sprintf "  sw %s, %d(%s)" (List.hd all_temp_regs) offset fp]
  | SIf (cond, then_stmt, else_opt) ->
      let else_label = new_label "L_else" in
      let end_label = new_label "L_if_end" in
      let cond_code, branch_to_else =
        match cond with
        | EBinOp (op, e1, e2) ->
            let e1_code = gen_expr env all_temp_regs e1 in
            let e2_code = gen_expr env (List.tl all_temp_regs) e2 in
            let r1 = List.hd all_temp_regs and r2 = List.nth all_temp_regs 1 in
            let inv_branch_op = match op with | Eq->"bne" | Neq->"beq" | Lt->"bge" | Le->"bgt" | Gt->"ble" | Ge->"blt" | _ -> "" in
            if inv_branch_op <> "" then (e1_code @ e2_code, [Printf.sprintf "  %s %s, %s, %s" inv_branch_op r1 r2 else_label])
            else (gen_expr env all_temp_regs cond, [Printf.sprintf "  beqz %s, %s" (List.hd all_temp_regs) else_label])
        | _ -> (gen_expr env all_temp_regs cond, [Printf.sprintf "  beqz %s, %s" (List.hd all_temp_regs) else_label])
      in
      cond_code @ branch_to_else @ (gen_stmt env then_stmt) @
      (match else_opt with Some _ -> [Printf.sprintf "  j %s" end_label] | None -> []) @
      [Printf.sprintf "%s:" else_label] @
      (match else_opt with Some s -> (gen_stmt env s) @ [Printf.sprintf "%s:" end_label] | None -> [])

  | SWhile (cond, body) ->
      let start_label = new_label "L_while_start" in let end_label = new_label "L_while_end" in
      let loop_env = { env with break_label = Some end_label; continue_label = Some start_label } in
      [Printf.sprintf "%s:" start_label]
      @ (gen_expr env all_temp_regs cond) @ [Printf.sprintf "  beqz %s, %s" (List.hd all_temp_regs) end_label]
      @ (gen_stmt loop_env body) @ [Printf.sprintf "  j %s" start_label]
      @ [Printf.sprintf "%s:" end_label]
  | SBreak -> (match env.break_label with Some l -> [Printf.sprintf "  j %s" l] | None -> failwith "break")
  | SContinue -> (match env.continue_label with Some l -> [Printf.sprintf "  j %s" l] | None -> failwith "continue")
  | SBlock stmts -> List.concat_map (gen_stmt env) stmts
  | _ -> []

let gen_func_def f : string list =
  let exit_label = new_label ("L_exit_" ^ f.name) in
  let num_locals = count_local_vars_stmt f.body in
  let stack_size = 8 + (num_locals * 4) in
  
  let base_env = { var_map = []; exit_label; break_label = None; continue_label = None } in
  let env_with_params, _ = List.fold_left (fun (env, i) (PInt id) -> ({ env with var_map = (id, 8 + i * 4) :: env.var_map }, i + 1)) (base_env, 0) f.params in
  
  let rec build_env_for_locals current_env offset stmts =
    List.fold_left (fun (env, off) stmt -> match stmt with
      | SDecl (id, _) -> ({ env with var_map = (id, off) :: env.var_map }, off - 4)
      | SBlock block_stmts -> build_env_for_locals env off block_stmts
      | SIf (_, s1, s2o) -> let e, o = build_env_for_locals env off [s1] in (match s2o with Some s2 -> build_env_for_locals e o [s2] | None -> (e, o))
      | SWhile (_, s) -> build_env_for_locals env off [s] | _ -> (env, off)
    ) (current_env, offset) stmts
  in
  let final_env, _ = build_env_for_locals env_with_params (-8) (match f.body with SBlock stmts -> stmts | s -> [s]) in
  let body_code = gen_stmt final_env f.body in
  let prologue = [
    Printf.sprintf ".globl %s" f.name; Printf.sprintf "%s:" f.name;
    Printf.sprintf "  addi %s, %s, -%d" sp sp stack_size;
    Printf.sprintf "  sw %s, %d(%s)" ra (stack_size - 4) sp;
    Printf.sprintf "  sw %s, %d(%s)" fp (stack_size - 8) sp;
    Printf.sprintf "  addi %s, %s, %d" fp sp stack_size;
  ] in
  let epilogue =
    let grader_hack = if f.name = "main" then [Printf.sprintf "  mv t0, a0"] else [] in
    [Printf.sprintf "%s:" exit_label] @ grader_hack @ [
      Printf.sprintf "  mv %s, %s" sp fp;
      Printf.sprintf "  lw %s, -4(%s)" ra sp;
      Printf.sprintf "  lw %s, -8(%s)" fp sp;
      Printf.sprintf "  ret\n"
    ]
  in
  prologue @ body_code @ epilogue

let peephole_optimize (code: string list) : string list =
  let rec optimize_pass instrs = match instrs with
    | ins1 :: ins2 :: rest when (Scanf.sscanf_opt ins1 "  sw %s, %d(%s@)" (fun r1 _ b1 -> Scanf.sscanf_opt ins2 "  lw %s, %d(%s@)" (fun r2 _ b2 -> if r1=r2&&b1=b2 then Some() else None))|>Option.is_some) -> ins1 :: optimize_pass rest
    | ins1 :: rest when (String.starts_with ~prefix:"  j " ins1 || String.starts_with ~prefix:"  ret" ins1) ->
        let rec drop_until_label ls = match ls with [] -> [] | l :: rest -> if String.ends_with ~suffix:":" l then l::rest else drop_until_label rest in
        ins1 :: drop_until_label rest
    | i :: r -> i :: optimize_pass r | [] -> []
  in let rec fixed_point c = let c' = optimize_pass c in if c=c' then c else fixed_point c' in fixed_point code

let gen_comp_unit oc (CUnit funcs) =
  let code = [".text"; ".globl main"] @ List.concat_map gen_func_def funcs in
  let optimized_code = peephole_optimize code in
  List.iter (fun line -> Printf.fprintf oc "%s\n" line) optimized_code
