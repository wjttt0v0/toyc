open Ast

let sp = "sp"
let fp = "s0"
let ra = "ra"
let a0 = "a0"
let temp_regs = ["t0"; "t1"; "t2"; "t3"; "t4"; "t5"]

let label_counter = ref 0
let new_label prefix =
  incr label_counter;
  prefix ^ string_of_int !label_counter

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

type env = {
  var_map: (id * int) list;
  current_offset: int;
  exit_label: string;
  break_label: string option;
  continue_label: string option;
}

let find_var_offset id env = 
  try List.assoc id env.var_map 
  with Not_found -> failwith ("Undeclared variable: " ^ id)

let count_local_vars_stmt stmt =
  let rec count acc s = match s with
    | SDecl _ -> acc + 1
    | SBlock stmts -> List.fold_left count acc stmts
    | SIf (_, s1, s2_opt) ->
      let acc' = count acc s1 in
      (match s2_opt with Some s2 -> count acc' s2 | None -> acc')
    | SWhile (_, body) -> count acc body
    | _ -> acc
  in count 0 stmt

let rec gen_expr env regs expr : (string list * string list) =
  let target_reg, rest_regs = match regs with
    | hd :: tl -> (hd, tl)
    | [] -> failwith "Expression too complex: ran out of registers"
  in
  match eval_const_expr expr with
  | Some n -> ([Printf.sprintf "  li %s, %d" target_reg n], rest_regs)
  | None ->
      match expr with
      | EInt n -> ([Printf.sprintf "  li %s, %d" target_reg n], rest_regs)
      | EId id ->
          let offset = find_var_offset id env in
          ([Printf.sprintf "  lw %s, %d(%s)" target_reg offset fp], rest_regs)
      | EUnOp (op, e) ->
          let (e_code, available_regs) = gen_expr env regs e in
          let op_code = match op with
            | Neg -> Printf.sprintf "  neg %s, %s" target_reg target_reg
            | Not -> Printf.sprintf "  seqz %s, %s" target_reg target_reg
          in
          (e_code @ [op_code], available_regs)
      | EBinOp (op, e1, e2) ->
          if op = And then
            let false_label = new_label "L_and_false" in let end_label = new_label "L_and_end" in
            let (e1_code, _) = gen_expr env regs e1 in let (e2_code, regs_after_e2) = gen_expr env regs e2 in
            (e1_code @ [Printf.sprintf "  beqz %s, %s" target_reg false_label] @ e2_code
             @ [Printf.sprintf "  snez %s, %s" target_reg target_reg; Printf.sprintf "  j %s" end_label; Printf.sprintf "%s:" false_label; Printf.sprintf "  li %s, 0" target_reg; Printf.sprintf "%s:" end_label], regs_after_e2)
          else if op = Or then
            let true_label = new_label "L_or_true" in let end_label = new_label "L_or_end" in
            let (e1_code, _) = gen_expr env regs e1 in let (e2_code, regs_after_e2) = gen_expr env regs e2 in
             (e1_code @ [Printf.sprintf "  bnez %s, %s" target_reg true_label] @ e2_code
              @ [Printf.sprintf "  snez %s, %s" target_reg target_reg; Printf.sprintf "  j %s" end_label; Printf.sprintf "%s:" true_label; Printf.sprintf "  li %s, 1" target_reg; Printf.sprintf "%s:" end_label], regs_after_e2)
          else
            let (e1_code, regs_after_e1) = gen_expr env regs e1 in let r1 = target_reg in
            let (e2_code, regs_after_e2) = gen_expr env regs_after_e1 e2 in let r2 = List.hd regs_after_e1 in
            let op_code = match op with
              | Add -> Printf.sprintf "  add %s, %s, %s" r1 r1 r2 | Sub -> Printf.sprintf "  sub %s, %s, %s" r1 r1 r2
              | Mul -> Printf.sprintf "  mul %s, %s, %s" r1 r1 r2 | Div -> Printf.sprintf "  div %s, %s, %s" r1 r1 r2
              | Mod -> Printf.sprintf "  rem %s, %s, %s" r1 r1 r2 | Eq  -> Printf.sprintf "  sub %s, %s, %s; seqz %s, %s" r1 r1 r2 r1 r1
              | Neq -> Printf.sprintf "  sub %s, %s, %s; snez %s, %s" r1 r1 r2 r1 r1 | Lt  -> Printf.sprintf "  slt %s, %s, %s" r1 r1 r2
              | Gt  -> Printf.sprintf "  sgt %s, %s, %s" r1 r1 r2 | Le  -> Printf.sprintf "  sgt %s, %s, %s; xori %s, %s, 1" r1 r1 r2 r1 r1
              | Ge  -> Printf.sprintf "  slt %s, %s, %s; xori %s, %s, 1" r1 r1 r2 r1 r1 | _   -> "" 
            in (e1_code @ e2_code @ [op_code], regs_after_e2)
      | ECall (id, args) ->
          let stack_space = 4 * List.length temp_regs in
          let save_regs = List.mapi (fun i r -> Printf.sprintf "  sw %s, %d(%s)" r (i*4) sp) temp_regs in
          let adj_sp_down = [Printf.sprintf "  addi %s, %s, -%d" sp sp stack_space] @ save_regs in
          let arg_eval_code = List.fold_left (fun acc_code arg ->
              let (current_arg_code, _) = gen_expr env temp_regs arg in
              let push_code = [Printf.sprintf "  addi %s, %s, -4" sp sp; Printf.sprintf "  sw %s, 0(%s)" (List.hd temp_regs) sp] in
              acc_code @ current_arg_code @ push_code
            ) [] (List.rev args) in
          let call_instruction = [Printf.sprintf "  call %s" id] in
          let cleanup_code = if not (args = []) then [Printf.sprintf "  addi %s, %s, %d" sp sp (4 * List.length args)] else [] in
          let restore_regs = List.mapi (fun i r -> Printf.sprintf "  lw %s, %d(%s)" r (i*4) sp) temp_regs in
          let adj_sp_up = restore_regs @ [Printf.sprintf "  addi %s, %s, %d" sp sp stack_space] in
          let move_return_value = [Printf.sprintf "  mv %s, %s" target_reg a0] in
          (adj_sp_down @ arg_eval_code @ call_instruction @ cleanup_code @ adj_sp_up @ move_return_value, rest_regs)

and gen_stmt env stmt : string list =
  match stmt with
  | SExpr e -> fst (gen_expr env temp_regs e)
  | SReturn (Some e) -> 
      let (e_code, _) = gen_expr env temp_regs e in
      e_code @ [Printf.sprintf "  mv %s, %s" a0 (List.hd temp_regs); Printf.sprintf "  j %s" env.exit_label]
  | SReturn None -> [Printf.sprintf "  j %s" env.exit_label]
  | SDecl (id, e) | SAssign (id, e) ->
      let (e_code, _) = gen_expr env temp_regs e in
      let offset = find_var_offset id env in
      e_code @ [Printf.sprintf "  sw %s, %d(%s)" (List.hd temp_regs) offset fp]
  | SIf (cond, then_stmt, else_opt) ->
      let else_label = new_label "L_else" in let end_label = new_label "L_if_end" in
      let cond_gen_code = match cond with
        | EBinOp (op, e1, e2) ->
            let (e1_code, regs1) = gen_expr env temp_regs e1 in let (e2_code, _) = gen_expr env regs1 e2 in
            let r1 = List.hd temp_regs in let r2 = List.hd regs1 in
            let branch_op = match op with
              | Eq  -> "bne" | Neq -> "beq" | Lt  -> "bge" | Ge  -> "blt" | Gt  -> "ble" | Le  -> "bgt" | _ -> ""
            in if branch_op <> "" then e1_code @ e2_code @ [Printf.sprintf "  %s %s, %s, %s" branch_op r1 r2 else_label]
            else let (full_expr_code, _) = gen_expr env temp_regs cond in full_expr_code @ [Printf.sprintf "  beqz %s, %s" (List.hd temp_regs) else_label]
        | _ -> let (e_code, _) = gen_expr env temp_regs cond in e_code @ [Printf.sprintf "  beqz %s, %s" (List.hd temp_regs) else_label]
      in let then_code = gen_stmt env then_stmt in let else_code = match else_opt with Some s -> gen_stmt env s | None -> [] in
      cond_gen_code @ then_code @ (if else_code <> [] then [Printf.sprintf "  j %s" end_label] else []) @
      [Printf.sprintf "%s:" else_label] @ else_code @ (if else_code <> [] then [Printf.sprintf "%s:" end_label] else [])
  | SWhile (cond, body) ->
      let start_label = new_label "L_while_start" in let end_label = new_label "L_while_end" in
      let loop_env = { env with break_label = Some end_label; continue_label = Some start_label } in
      let (cond_code, _) = gen_expr env temp_regs cond in
      [Printf.sprintf "%s:" start_label] @ cond_code @ [Printf.sprintf "  beqz %s, %s" (List.hd temp_regs) end_label]
      @ (gen_stmt loop_env body) @ [Printf.sprintf "  j %s" start_label] @ [Printf.sprintf "%s:" end_label]
  | SBreak -> (match env.break_label with Some l -> [Printf.sprintf "  j %s" l] | None -> failwith "break used outside a loop")
  | SContinue -> (match env.continue_label with Some l -> [Printf.sprintf "  j %s" l] | None -> failwith "continue used outside a loop")
  | SBlock stmts -> List.concat_map (gen_stmt env) stmts

(*** START OF MODIFIED SECTION ***)
let gen_func_def f : string list =
  let exit_label = new_label ("L_exit_" ^ f.name) in
  let num_locals = count_local_vars_stmt f.body in
  let num_params = List.length f.params in
  
  (* Total stack space needed for local variables AND saved parameters *)
  let locals_and_params_space = (num_locals + num_params) * 4 in
  let stack_size = 8 (* ra & old fp *) + locals_and_params_space in

  (* Create the initial environment and code to save parameters from registers to stack *)
  let initial_offset = -8 in
  let (env_with_params, save_params_code, offset_after_params) =
    List.fold_left (fun (env, code, offset) (param_idx, PInt id) ->
      if param_idx >= 8 then failwith "More than 8 parameters not supported";
      let arg_reg = "a" ^ string_of_int param_idx in
      let save_inst = Printf.sprintf "  sw %s, %d(%s)" arg_reg offset fp in
      let new_env = { env with var_map = (id, offset) :: env.var_map } in
      (new_env, code @ [save_inst], offset - 4)
    ) ({ var_map = []; current_offset=0; exit_label; break_label=None; continue_label=None }, [], initial_offset) 
      (List.mapi (fun i p -> (i, p)) f.params)
  in

  (* Now, find offsets for SDecl locals, starting from where params left off *)
  let (final_env, _) =
    let rec build_env_for_decls current_env offset stmt_list =
      List.fold_left (fun (env, off) stmt ->
        match stmt with
        | SDecl(id, _) -> ({ env with var_map=(id, off)::env.var_map }, off - 4)
        | SBlock stmts -> build_env_for_decls env off stmts
        | SIf (_, s1, s2o) ->
            let (env', off') = build_env_for_decls env off [s1] in
            (match s2o with Some s2 -> build_env_for_decls env' off' [s2] | None -> (env', off'))
        | SWhile (_, s) -> build_env_for_decls env off [s]
        | _ -> (env, off)
      ) (current_env, offset) stmt_list
    in
    build_env_for_decls env_with_params offset_after_params (match f.body with SBlock stmts -> stmts | s -> [s])
  in
  
  let body_code = gen_stmt final_env f.body in

  [Printf.sprintf ".globl %s" f.name; Printf.sprintf "%s:" f.name]
  (* Prologue *)
  @ [ Printf.sprintf "  addi %s, %s, -%d" sp sp stack_size;
      Printf.sprintf "  sw %s, %d(%s)" ra (stack_size - 4) sp;
      Printf.sprintf "  sw %s, %d(%s)" fp (stack_size - 8) sp;
      Printf.sprintf "  addi %s, %s, %d" fp sp stack_size;
    ]
  @ save_params_code (* <-- This is the new, crucial part *)
  (* Body *)
  @ body_code
  (* Epilogue *)
  @ [ Printf.sprintf "%s:" exit_label;
      Printf.sprintf "  addi %s, %s, -%d" sp fp stack_size;
      Printf.sprintf "  lw %s, %d(%s)" ra (stack_size-4) sp;
      Printf.sprintf "  lw %s, %d(%s)" fp (stack_size-8) sp;
      Printf.sprintf "  addi %s, %s, %d" sp sp stack_size;
      Printf.sprintf "  ret"
    ]
(*** END OF MODIFIED SECTION ***)

let peephole_optimize (code: string list) : string list =
  let rec optimize_pass instrs = match instrs with
    | ins1 :: ins2 :: rest when (Scanf.sscanf_opt ins1 "  sw %s, %d(%s@)" (fun r1 n1 b1 -> Scanf.sscanf_opt ins2 "  lw %s, %d(%s@)" (fun r2 n2 b2 -> if r1 = r2 && n1 = n2 && b1 = b2 then Some () else None)) |> Option.is_some) -> ins1 :: (optimize_pass rest)
    | ins1 :: rest when (String.starts_with ~prefix:"  j" ins1 || String.starts_with ~prefix:"  ret" ins1) -> let rec drop_unreachable remaining = match remaining with [] -> [] | l :: ls when String.ends_with ~suffix:":" l -> l :: ls | _ :: ls -> drop_unreachable ls in ins1 :: (optimize_pass (drop_unreachable rest))
    | ins :: rest -> ins :: (optimize_pass rest)
    | [] -> []
  in let prev_code_ref = ref [] in let next_code_ref = ref code in while !next_code_ref <> !prev_code_ref do prev_code_ref := !next_code_ref; next_code_ref := optimize_pass !next_code_ref done; !next_code_ref

let gen_comp_unit oc (CUnit funcs) =
  let unoptimized_code = [".text"] @ List.concat_map gen_func_def funcs in
  let optimized_code = peephole_optimize unoptimized_code in
  List.iter (fun line -> Printf.fprintf oc "%s\n" line) optimized_code