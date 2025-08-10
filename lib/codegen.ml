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

type env = {
  var_map: (id, int) Hashtbl.t;
  mutable current_offset: int;
  exit_label: string;
  break_label: string option;
  continue_label: string option;
}

let find_var_offset id env = 
  try Hashtbl.find env.var_map id 
  with Not_found -> failwith ("Undeclared variable: " ^ id)

let add_var env id =
  env.current_offset <- env.current_offset - 4;
  Hashtbl.add env.var_map id env.current_offset;
  env.current_offset

let rec gen_expr env regs expr : (string list * string list) =
  let target_reg, rest_regs = match regs with
    | hd :: tl -> (hd, tl)
    | [] -> failwith "Compiler Error: Register allocation exhausted unexpectedly."
  in
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
      let (e1_code, regs_after_e1) = gen_expr env regs e1 in
      let r1 = target_reg in
      
      let (e2_code, regs_after_e2) =
        (* This is the core of register spilling logic *)
        if regs_after_e1 <> [] then
          gen_expr env regs_after_e1 e2
        else
          let spill_code = [
            Printf.sprintf "  addi %s, %s, -4" sp sp;
            Printf.sprintf "  sw %s, 0(%s)" r1 sp;
          ] in
          let (e2_eval_code, _) = gen_expr env regs e2 in
          let restore_code = [
            Printf.sprintf "  lw %s, 0(%s)" r1 sp; (* Restore to a different reg to avoid overwrite *)
            Printf.sprintf "  addi %s, %s, 4" sp sp;
          ] in
          (* Note: After spilling, we pretend we used up all regs *)
          (spill_code @ e2_eval_code, [])
      in
      
      let r2 = if regs_after_e1 <> [] then List.hd regs_after_e1 else target_reg in
      let op_code = match op with
        | Add -> Printf.sprintf "  add %s, %s, %s" r1 r1 r2 | Sub -> Printf.sprintf "  sub %s, %s, %s" r1 r1 r2
        | Mul -> Printf.sprintf "  mul %s, %s, %s" r1 r1 r2 | Div -> Printf.sprintf "  div %s, %s, %s" r1 r1 r2
        | Mod -> Printf.sprintf "  rem %s, %s, %s" r1 r1 r2 | Eq  -> Printf.sprintf "  sub %s, %s, %s; seqz %s, %s" r1 r1 r2 r1 r1
        | Neq -> Printf.sprintf "  sub %s, %s, %s; snez %s, %s" r1 r1 r2 r1 r1 | Lt  -> Printf.sprintf "  slt %s, %s, %s" r1 r1 r2
        | Gt  -> Printf.sprintf "  sgt %s, %s, %s" r1 r1 r2 | Le  -> Printf.sprintf "  sgt %s, %s, %s; xori %s, %s, 1" r1 r1 r2 r1 r1
        | Ge  -> Printf.sprintf "  slt %s, %s, %s; xori %s, %s, 1" r1 r1 r2 r1 r1
        | And -> Printf.sprintf "  and %s, %s, %s; snez %s, %s" r1 r1 r2 r1 r1 (* Simplified non-short-circuit version *)
        | Or  -> Printf.sprintf "  or %s, %s, %s; snez %s, %s" r1 r1 r2 r1 r1  (* Simplified non-short-circuit version *)
      in
      (e1_code @ e2_code @ [op_code], regs_after_e2)

  | ECall (id, args) ->
      let arg_eval_code = List.fold_left (fun acc_code arg ->
          let (current_arg_code, _) = gen_expr env all_temp_regs arg in
          let push_code = [
              Printf.sprintf "  addi %s, %s, -4" sp sp;
              Printf.sprintf "  sw %s, 0(%s)" (List.hd all_temp_regs) sp
            ] in
          acc_code @ current_arg_code @ push_code
        ) [] (List.rev args) in
      let call_instruction = [Printf.sprintf "  call %s" id] in
      let cleanup_code =
          let arg_space = 4 * List.length args in
          if arg_space > 0 then [Printf.sprintf "  addi %s, %s, %d" sp sp arg_space] else []
      in
      let move_return_value = [Printf.sprintf "  mv %s, %s" target_reg a0] in
      (arg_eval_code @ call_instruction @ cleanup_code @ move_return_value, rest_regs)

and gen_stmt env stmt : string list =
  match stmt with
  | SExpr e -> fst (gen_expr env all_temp_regs e)
  | SReturn (Some e) -> 
      let (e_code, _) = gen_expr env all_temp_regs e in
      e_code @ [Printf.sprintf "  mv %s, %s" a0 (List.hd all_temp_regs); Printf.sprintf "  j %s" env.exit_label]
  | SReturn None -> [Printf.sprintf "  j %s" env.exit_label]
  | SDecl (id, e) ->
      let offset = add_var env id in
      let (e_code, _) = gen_expr env all_temp_regs e in
      e_code @ [Printf.sprintf "  sw %s, %d(%s)" (List.hd all_temp_regs) offset fp]
  | SAssign (id, e) ->
      let offset = find_var_offset id env in
      let (e_code, _) = gen_expr env all_temp_regs e in
      e_code @ [Printf.sprintf "  sw %s, %d(%s)" (List.hd all_temp_regs) offset fp]
  | SIf (cond, then_stmt, else_opt) ->
      let else_label = new_label "L_else" in let end_label = new_label "L_if_end" in
      let (cond_code, _) = gen_expr env all_temp_regs cond in
      let then_code = gen_stmt env then_stmt in
      let else_code_opt = Option.map (gen_stmt env) else_opt in
      cond_code @ [Printf.sprintf "  beqz %s, %s" (List.hd all_temp_regs) else_label] @
      then_code @ (match else_code_opt with Some _ -> [Printf.sprintf "  j %s" end_label] | None -> []) @
      [Printf.sprintf "%s:" else_label] @
      (match else_code_opt with Some s -> s | None -> []) @
      [Printf.sprintf "%s:" end_label]
  | SWhile (cond, body) ->
      let start_label = new_label "L_while_start" in let end_label = new_label "L_while_end" in
      let loop_env = { env with break_label = Some end_label; continue_label = Some start_label } in
      let (cond_code, _) = gen_expr loop_env all_temp_regs cond in
      [Printf.sprintf "%s:" start_label] @ cond_code
      @ [Printf.sprintf "  beqz %s, %s" (List.hd all_temp_regs) end_label]
      @ (gen_stmt loop_env body) @ [Printf.sprintf "  j %s" start_label] @ [Printf.sprintf "%s:" end_label]
  | SBreak -> (match env.break_label with Some l -> [Printf.sprintf "  j %s" l] | None -> failwith "break used outside a loop")
  | SContinue -> (match env.continue_label with Some l -> [Printf.sprintf "  j %s" l] | None -> failwith "continue used outside a loop")
  | SBlock stmts -> List.concat_map (gen_stmt env) stmts
  | Sempty -> [] (* For ";" statement *)


let gen_func_def f : string list =
  let env = {
    var_map = Hashtbl.create 16;
    current_offset = 0;
    exit_label = new_label ("L_exit_" ^ f.name);
    break_label = None; continue_label = None;
  } in

  (* Simplified: Just pre-calculate all local variable space *)
  let rec count_locals stmt = match stmt with
    | SDecl _ -> 1
    | SBlock stmts -> List.fold_left (+) 0 (List.map count_locals stmts)
    | SIf (_, s1, s2o) -> count_locals s1 + (match s2o with Some s -> count_locals s | None -> 0)
    | SWhile (_, b) -> count_locals b
    | _ -> 0
  in
  let num_locals = count_locals f.body in
  let stack_locals_space = num_locals * 4 in

  (* Assign offsets for parameters (positive relative to fp) *)
  List.iteri (fun i (PInt id) ->
    let offset = 8 + i * 4 in
    Hashtbl.add env.var_map id offset
  ) f.params;

  let body_code = gen_stmt env f.body in

  let stack_size = 8 + stack_locals_space in
  
  [Printf.sprintf ".globl %s" f.name; Printf.sprintf "%s:" f.name]
  @ [ Printf.sprintf "  addi %s, %s, -%d" sp sp stack_size;
      Printf.sprintf "  sw %s, %d(%s)" ra (stack_size - 4) sp;
      Printf.sprintf "  sw %s, %d(%s)" fp (stack_size - 8) sp;
      Printf.sprintf "  addi %s, %s, %d" fp sp stack_size;
  ]
  @ (if env.current_offset <> 0 then [Printf.sprintf "  addi %s, %s, %d" sp sp env.current_offset] else [])
  @ body_code
  @ [ Printf.sprintf "%s:" env.exit_label;
      Printf.sprintf "  mv %s, %s" sp fp;
      Printf.sprintf "  lw %s, -4(%s)" ra sp;
      Printf.sprintf "  lw %s, -8(%s)" fp sp;
      Printf.sprintf "  ret"
    ]

let gen_comp_unit oc (CUnit funcs) =
  let code = [".text"] @ List.concat_map gen_func_def funcs in
  List.iter (fun line -> Printf.fprintf oc "%s\n" line) code