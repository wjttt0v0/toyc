open Ast

(* 寄存器别名 *)
let sp = "sp"    (* Stack Pointer *)
let fp = "s0"    (* Frame Pointer *)
let ra = "ra"    (* Return Address *)
let a0 = "a0"    (* Argument 0 / Return Value *)
let t0 = "t0"    (* Temporary 0 *)
let t1 = "t1"    (* Temporary 1 *)
let t2 = "t2"    (* Temporary 2 *)

(* 唯一标签生成器 *)
let label_counter = ref 0
let new_label prefix =
  incr label_counter;
  prefix ^ string_of_int !label_counter

(* 代码生成环境 *)
type env = {
  (* 变量名 -> 栈上相对于 fp 的偏移量 *)
  var_map: (id * int) list;
  (* 当前栈顶相对于 fp 的偏移量，用于分配新变量，通常为负数 *)
  current_offset: int;
  (* 当前函数的退出标签，用于 return 语句跳转 *)
  exit_label: string;
  (* 当前循环的 break/continue 标签 *)
  break_label: string option;
  continue_label: string option;
}

(* 查找变量在栈上的偏移 *)
let find_var_offset id env =
  try List.assoc id env.var_map
  with Not_found -> failwith ("Semantic Error: Undeclared variable " ^ id)

(* 遍历 AST，计算函数需要为局部变量分配的总空间大小 *)
let rec count_local_vars_stmt stmt =
  match stmt with
  | SDecl _ -> 1
  | SBlock stmts -> List.fold_left (fun acc s -> acc + count_local_vars_stmt s) 0 stmts
  | SIf (_, s1, s2_opt) ->
      let count1 = count_local_vars_stmt s1 in
      let count2 = match s2_opt with Some s -> count_local_vars_stmt s | None -> 0 in
      count1 + count2
  | SWhile (_, body) -> count_local_vars_stmt body
  | _ -> 0

(* 生成表达式的代码，结果存放在 t0 寄存器中 *)
let rec gen_expr oc env expr =
  match expr with
  | EInt n ->
      Printf.fprintf oc "  li %s, %d\n" t0 n
  | EId id ->
      let offset = find_var_offset id env in
      Printf.fprintf oc "  lw %s, %d(%s)\n" t0 offset fp
  | EUnOp (op, e) ->
      gen_expr oc env e;
      (match op with
      | Neg -> Printf.fprintf oc "  neg %s, %s\n" t0 t0
      | Not -> Printf.fprintf oc "  seqz %s, %s\n" t0 t0)
  | EBinOp (op, e1, e2) ->
      (* 特殊处理短路求值 *)
      (match op with
      | And ->
          let false_label = new_label "L_and_false" in
          let end_label = new_label "L_and_end" in
          gen_expr oc env e1;
          Printf.fprintf oc "  beqz %s, %s\n" t0 false_label; (* 如果 e1 是 false，则结果是 false *)
          gen_expr oc env e2;
          Printf.fprintf oc "  snez %s, %s\n" t0 t0; (* 将 e2 的结果转换为 0 或 1 *)
          Printf.fprintf oc "  j %s\n" end_label;
          Printf.fprintf oc "%s:\n" false_label;
          Printf.fprintf oc "  li %s, 0\n" t0; (* 结果是 false (0) *)
          Printf.fprintf oc "%s:\n" end_label
      | Or ->
          let true_label = new_label "L_or_true" in
          let end_label = new_label "L_or_end" in
          gen_expr oc env e1;
          Printf.fprintf oc "  bnez %s, %s\n" t0 true_label; (* 如果 e1 是 true，则结果是 true *)
          gen_expr oc env e2;
          Printf.fprintf oc "  snez %s, %s\n" t0 t0; (* 将 e2 的结果转换为 0 或 1 *)
          Printf.fprintf oc "  j %s\n" end_label;
          Printf.fprintf oc "%s:\n" true_label;
          Printf.fprintf oc "  li %s, 1\n" t0; (* 结果是 true (1) *)
          Printf.fprintf oc "%s:\n" end_label
      | _ ->
          gen_expr oc env e1;
          Printf.fprintf oc "  addi %s, %s, -4\n" sp sp; (* 为 e1 的结果在栈上分配临时空间 *)
          Printf.fprintf oc "  sw %s, 0(%s)\n" t0 sp;
          gen_expr oc env e2;
          Printf.fprintf oc "  mv %s, %s\n" t1 t0;     (* e2 的结果在 t1 *)
          Printf.fprintf oc "  lw %s, 0(%s)\n" t0 sp;     (* e1 的结果恢复到 t0 *)
          Printf.fprintf oc "  addi %s, %s, 4\n" sp sp;  (* 回收临时空间 *)
          (match op with
          | Add -> Printf.fprintf oc "  add %s, %s, %s\n" t0 t0 t1
          | Sub -> Printf.fprintf oc "  sub %s, %s, %s\n" t0 t0 t1
          | Mul -> Printf.fprintf oc "  mul %s, %s, %s\n" t0 t0 t1
          | Div -> Printf.fprintf oc "  div %s, %s, %s\n" t0 t0 t1
          | Mod -> Printf.fprintf oc "  rem %s, %s, %s\n" t0 t0 t1
          | Eq  -> Printf.fprintf oc "  sub %s, %s, %s\n  seqz %s, %s\n" t0 t0 t1 t0 t0
          | Neq -> Printf.fprintf oc "  sub %s, %s, %s\n  snez %s, %s\n" t0 t0 t1 t0 t0
          | Lt  -> Printf.fprintf oc "  slt %s, %s, %s\n" t0 t0 t1
          | Le  -> Printf.fprintf oc "  sgt %s, %s, %s\n  xori %s, %s, 1\n" t0 t0 t1 t0 t0
          | Gt  -> Printf.fprintf oc "  sgt %s, %s, %s\n" t0 t0 t1
          | Ge  -> Printf.fprintf oc "  slt %s, %s, %s\n  xori %s, %s, 1\n" t0 t0 t1 t0 t0
          | And | Or -> assert false (* Should have been handled above *)
          )
      )
  | ECall (id, args) ->
      (* 1. 计算所有参数值，并压入栈中 *)
      let arg_space = 4 * List.length args in
      if arg_space > 0 then Printf.fprintf oc "  addi %s, %s, -%d\n" sp sp arg_space;
      List.iteri (fun i arg ->
        gen_expr oc env arg;
        let arg_offset = i * 4 in
        Printf.fprintf oc "  sw %s, %d(%s)\n" t0 arg_offset sp;
      ) args;
      
      (* 2. 调用函数 *)
      Printf.fprintf oc "  call %s\n" id;

      (* 3. 从栈中清理参数 *)
      if arg_space > 0 then Printf.fprintf oc "  addi %s, %s, %d\n" sp sp arg_space;

      (* 4. 返回值已经在 a0, 移动到 t0 中以供后续表达式使用 *)
      Printf.fprintf oc "  mv %s, %s\n" t0 a0

(* 生成语句的代码 *)
let rec gen_stmt oc env stmt =
  match stmt with
  | SExpr e ->
      gen_expr oc env e (* 结果被丢弃 *)
  | SReturn (Some e) ->
      gen_expr oc env e;
      Printf.fprintf oc "  mv %s, %s\n" a0 t0; (* 将返回值放入 a0 *)
      Printf.fprintf oc "  j %s\n" env.exit_label
  | SReturn None ->
      Printf.fprintf oc "  j %s\n" env.exit_label
  | SDecl (id, e) ->
      gen_expr oc env e;
      let offset = find_var_offset id env in
      Printf.fprintf oc "  sw %s, %d(%s)\n" t0 offset fp
  | SAssign (id, e) ->
      gen_expr oc env e;
      let offset = find_var_offset id env in
      Printf.fprintf oc "  sw %s, %d(%s)\n" t0 offset fp
  
  | SIf (cond, then_stmt, else_opt) ->
      (match else_opt with
      | Some else_stmt ->
          (* if-then-else 结构 *)
          let else_label = new_label "L_else" in
          let end_label = new_label "L_if_end" in
          gen_expr oc env cond;
          Printf.fprintf oc "  beqz %s, %s\n" t0 else_label; (* cond为假, 跳转到else *)
          gen_stmt oc env then_stmt; (* then 分支 *)
          Printf.fprintf oc "  j %s\n" end_label;       (* then 分支结束, 跳过else *)
          Printf.fprintf oc "%s:\n" else_label;
          gen_stmt oc env else_stmt; (* else 分支 *)
          Printf.fprintf oc "%s:\n" end_label
      | None ->
          (* if-then 结构 *)
          let end_label = new_label "L_if_end" in
          gen_expr oc env cond;
          Printf.fprintf oc "  beqz %s, %s\n" t0 end_label; (* cond为假, 直接跳过then *)
          gen_stmt oc env then_stmt; (* then 分支 *)
          Printf.fprintf oc "%s:\n" end_label
      )

  | SWhile (cond, body) ->
      let start_label = new_label "L_while_start" in
      let end_label = new_label "L_while_end" in
      let loop_env = { env with break_label = Some end_label; continue_label = Some start_label } in
      Printf.fprintf oc "%s:\n" start_label; (* 循环开始/continue跳转点 *)
      gen_expr oc env cond;
      Printf.fprintf oc "  beqz %s, %s\n" t0 end_label; (* 条件为假, 退出循环 *)
      gen_stmt oc loop_env body; (* 循环体 *)
      Printf.fprintf oc "  j %s\n" start_label; (* 无条件跳回循环开始处 *)
      Printf.fprintf oc "%s:\n" end_label (* 循环结束/break跳转点 *)

  | SBreak ->
      (match env.break_label with
      | Some label -> Printf.fprintf oc "  j %s\n" label
      | None -> failwith "Semantic Error: 'break' outside of loop")
  | SContinue ->
      (match env.continue_label with
      | Some label -> Printf.fprintf oc "  j %s\n" label
      | None -> failwith "Semantic Error: 'continue' outside of loop")
  
  | SBlock stmts ->
      (* 块作用域: 计算块内声明的变量数并分配栈空间 *)
      let locals_in_block = count_local_vars_stmt (SBlock stmts) in
      let block_space = locals_in_block * 4 in
      if block_space > 0 then Printf.fprintf oc "  addi %s, %s, -%d\n" sp sp block_space;
      
      (* 为块内变量创建新的环境, 更新偏移量 *)
      let block_env = 
        let new_offset = env.current_offset - block_space in
        let (updated_var_map, _) = List.fold_left (fun (map, off) s ->
          match s with
          | SDecl(id, _) ->
              let new_off = off - 4 in
              ((id, new_off) :: map, new_off)
          | _ -> (map, off)
        ) (env.var_map, env.current_offset) stmts in
        { env with var_map = updated_var_map; current_offset = new_offset }
      in
      
      List.iter (gen_stmt oc block_env) stmts;

      (* 退出块作用域: 回收栈空间 *)
      if block_space > 0 then Printf.fprintf oc "  addi %s, %s, %d\n" sp sp block_space

(* 生成单个函数的代码 *)
let gen_func_def oc f =
  (* 0. 创建初始环境和标签 *)
  let exit_label = new_label ("L_exit_" ^ f.name) in

  (* 1. 函数序言 (Prologue) *)
  Printf.fprintf oc "%s:\n" f.name;
  (* 保存返回地址 (ra) 和调用者的帧指针 (fp) *)
  Printf.fprintf oc "  addi %s, %s, -8\n" sp sp;
  Printf.fprintf oc "  sw %s, 4(%s)\n" ra sp;
  Printf.fprintf oc "  sw %s, 0(%s)\n" fp sp;
  (* 设置新的帧指针 *)
  Printf.fprintf oc "  mv %s, %s\n" fp sp;

  (* 2. 为所有局部变量一次性分配栈空间 *)
  let num_locals = count_local_vars_stmt f.body in
  let locals_space = num_locals * 4 in
  if locals_space > 0 then Printf.fprintf oc "  addi %s, %s, -%d\n" sp sp locals_space;

  (* 3. 构建包含参数和局部变量的完整环境 *)
  (* 参数位于 fp 正向偏移处: 8(fp), 12(fp), ... *)
  let env_with_params, _ = List.fold_left (fun (env, i) (PInt id) ->
    let param_offset = 8 + i * 4 in
    ({env with var_map = (id, param_offset) :: env.var_map}, i + 1)
  ) (({ var_map = []; current_offset = 0; exit_label; break_label=None; continue_label=None }), 0) f.params
  in
  (* 局部变量位于 fp 负向偏移处 *)
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
    ) (env_with_params, current_offset) stmts
  in
  let final_env, _ = build_env_for_locals 0 (match f.body with SBlock stmts -> stmts | s -> [s]) in
  
  (* 4. 生成函数体代码 *)
  gen_stmt oc final_env f.body;

  (* 5. 函数结尾 (Epilogue) *)
  Printf.fprintf oc "%s:\n" exit_label;
  (* 恢复 sp, 使其指向 fp *)
  Printf.fprintf oc "  mv %s, %s\n" sp fp;
  (* 恢复 ra 和 fp *)
  Printf.fprintf oc "  lw %s, 4(%s)\n" ra sp;
  Printf.fprintf oc "  lw %s, 0(%s)\n" fp sp;
  Printf.fprintf oc "  addi %s, %s, 8\n" sp sp;
  (* 返回 *)
  Printf.fprintf oc "  ret\n\n"

(* 生成整个编译单元的代码 *)
let gen_comp_unit oc (CUnit funcs) =
  List.iter (gen_func_def oc) funcs