(* ir_gen.ml *)

open Ast
open Ir

(* 转换器上下文，用于在递归下降过程中传递状态 *)
type context = {
  temp_counter: int ref;          (* 临时变量计数器 *)
  label_counter: int ref;         (* 标签计数器 *)
  return_label: string;           (* 当前函数的返回标签 *)
  break_label: string option;     (* 当前循环的 break 标签 *)
  continue_label: string option;  (* 当前循环的 continue 标签 *)
}

(* 创建一个新的临时操作数 *)
let new_temp ctx =
  incr ctx.temp_counter;
  Temp !(ctx.temp_counter)

(* 创建一个新标签的字符串 *)
let new_label ctx prefix =
  incr ctx.label_counter;
  prefix ^ string_of_int !(ctx.label_counter)

(* === 表达式到 IR 的转换 === *)
(*
 * 将 AST 表达式转换为一系列 IR 指令和一个最终存放结果的操作数。
 * 返回: (指令列表, 结果操作数)
 *)
let rec gen_expr_ir ctx expr : instruction list * operand =
  match expr with
  | EInt n ->
      (* 常量：没有指令，操作数就是它本身 *)
      ([], Const n)

  | EId id ->
      (* 变量：没有指令，操作数就是它本身 *)
      ([], Var id)

  | EUnOp (op, e) ->
      let (e_instrs, e_op) = gen_expr_ir ctx e in
      let dest = new_temp ctx in
      let new_instr = UnaryOp (dest, op, e_op) in
      (e_instrs @ [new_instr], dest)

  | EBinOp (op, e1, e2) ->
      (* 为 && 和 || 实现短路求值 *)
      (match op with
      | And ->
          let dest = new_temp ctx in
          let false_label = new_label ctx "L_and_false" in
          let end_label = new_label ctx "L_and_end" in
          let (e1_instrs, e1_op) = gen_expr_ir ctx e1 in
          let (e2_instrs, e2_op) = gen_expr_ir ctx e2 in

          let instrs =
            e1_instrs
            @ [ IfZero (e1_op, false_label) ] (* if e1 is 0, result is 0 *)
            @ e2_instrs
            @ [ Assign (dest, e2_op) ]         (* result is e2 *)
            @ [ BinaryOp(dest, dest, Neq, Const 0) ] (* a = (a != 0) *)
            @ [ GoTo end_label ]
            @ [ Label false_label ]
            @ [ Assign (dest, Const 0) ]
            @ [ Label end_label ]
          in
          (instrs, dest)
      | Or ->
          let dest = new_temp ctx in
          let true_label = new_label ctx "L_or_true" in
          let end_label = new_label ctx "L_or_end" in
          let (e1_instrs, e1_op) = gen_expr_ir ctx e1 in
          let (e2_instrs, e2_op) = gen_expr_ir ctx e2 in

          let instrs =
            e1_instrs
            @ [ IfZero (e1_op, new_label ctx "L_or_check_e2") ] (* if e1 is 0, check e2 *)
            @ [ Assign (dest, Const 1) ] (* e1 is non-zero, result is 1 *)
            @ [ GoTo end_label ]
            @ [ Label (new_label ctx "L_or_check_e2") ]
            @ e2_instrs
            @ [ Assign (dest, e2_op) ]
            @ [ BinaryOp(dest, dest, Neq, Const 0) ] (* a = (a != 0) *)
            @ [ Label end_label ]
          in
          (instrs, dest)
      | _ ->
          (* 普通二元运算 *)
          let (e1_instrs, e1_op) = gen_expr_ir ctx e1 in
          let (e2_instrs, e2_op) = gen_expr_ir ctx e2 in
          let dest = new_temp ctx in
          let new_instr = BinaryOp (dest, e1_op, op, e2_op) in
          (e1_instrs @ e2_instrs @ [new_instr], dest))

  | ECall (id, args) ->
      (* 1. 计算所有参数 *)
      let (args_instrs, args_ops) =
        List.map (gen_expr_ir ctx) args |> List.split in
      let all_args_instrs = List.flatten args_instrs in
      
      (* 2. 创建 param 指令 *)
      let param_instrs = List.map (fun arg_op -> Param arg_op) args_ops in

      (* 3. 创建 call 指令，总是假设它有返回值，如果不用会被丢弃 *)
      let dest = new_temp ctx in
      let call_instr = Call (Some dest, id, args_ops) in (* 注意：这里的参数列表可能在codegen中有用 *)

      (all_args_instrs @ param_instrs @ [call_instr], dest)

(* === 语句到 IR 的转换 === *)
(*
 * 将 AST 语句转换为一系列 IR 指令。
 * 返回: 指令列表
 *)
let rec gen_stmt_ir ctx stmt : instruction list =
  match stmt with
  | SExpr e ->
      (* 计算表达式，然后丢弃其结果 *)
      let (instrs, _) = gen_expr_ir ctx e in
      instrs

  | SReturn (Some e) ->
      let (e_instrs, e_op) = gen_expr_ir ctx e in
      e_instrs @ [ Return (Some e_op); GoTo ctx.return_label ]

  | SReturn None ->
      [ Return None; GoTo ctx.return_label ]

  | SDecl (id, e) | SAssign (id, e) ->
      let (e_instrs, e_op) = gen_expr_ir ctx e in
      e_instrs @ [ Assign (Var id, e_op) ]

  | SIf (cond, then_stmt, else_opt) ->
      let (cond_instrs, cond_op) = gen_expr_ir ctx cond in
      let else_label = new_label ctx "L_else" in
      let end_label = new_label ctx "L_if_end" in

      let then_instrs = gen_stmt_ir ctx then_stmt in
      let else_instrs =
        match else_opt with
        | Some s -> gen_stmt_ir ctx s
        | None -> []
      in

      cond_instrs
      @ [ IfZero (cond_op, else_label) ]
      @ then_instrs
      @ [ GoTo end_label ]
      @ [ Label else_label ]
      @ else_instrs
      @ [ Label end_label ]

  | SWhile (cond, body) ->
      let start_label = new_label ctx "L_while_start" in
      let body_label = new_label ctx "L_while_body" in
      let end_label = new_label ctx "L_while_end" in
      
      (* 创建一个包含 break/continue 标签的新上下文 *)
      let loop_ctx = { ctx with break_label = Some end_label; continue_label = Some start_label } in
      
      let (cond_instrs, cond_op) = gen_expr_ir loop_ctx cond in
      let body_instrs = gen_stmt_ir loop_ctx body in

      [ Label start_label ]
      @ cond_instrs
      @ [ IfZero (cond_op, end_label) ] (* if cond is false, exit loop *)
      @ body_instrs
      @ [ GoTo start_label ]  (* loop back *)
      @ [ Label end_label ]

  | SBreak ->
      (match ctx.break_label with
       | Some label -> [ GoTo label ]
       | None -> failwith "IR Generation Error: 'break' used outside a loop.")

  | SContinue ->
      (match ctx.continue_label with
       | Some label -> [ GoTo label ]
       | None -> failwith "IR Generation Error: 'continue' used outside a loop.")

  | SBlock stmts ->
      List.concat_map (gen_stmt_ir ctx) stmts

(* === 函数定义到 IR 的转换 === *)
let gen_func_def_ir (f: Ast.func_def) : ir_function =
  (* 为每个函数创建独立的上下文 *)
  let ctx = {
    temp_counter = ref 0;
    label_counter = ref 0;
    return_label = "L_exit_" ^ f.name;
    break_label = None;
    continue_label = None;
  } in

  (* 将 AST 参数转换为 IR 操作数 *)
  let ir_params = List.map (function PInt id -> Var id) f.params in
  
  (* 生成函数体的 IR 指令 *)
  let body_instrs = gen_stmt_ir ctx f.body in

  (* 组合成最终的指令列表 *)
  let final_instrs =
    body_instrs
    @ [ Label ctx.return_label ]
    (* 为确保 void 函数总有返回路径，添加一个最终的 Return。
       对于有返回值的函数，由于前面的 GoTo，这条指令通常是不可达的。
       更复杂的流分析可以移除它。*)
    @ (if f.return_type = TVoid then [ Return None ] else [])
  in

  { name = f.name; params = ir_params; instrs = final_instrs }

(* === 主转换函数 === *)
let gen_ir (CUnit funcs) : ir_program =
  List.map gen_func_def_ir funcs