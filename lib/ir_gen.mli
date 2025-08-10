(* ir_gen.mli *)

(**
 * AST 到 IR 的转换器
 *
 * 将源程序的抽象语法树 (AST) 转换为中间表示 (IR)。
 *)

(**
 * 将一个完整的编译单元 (AST) 转换为一个 IR 程序。
 * @param cu AST 根节点 Ast.comp_unit
 * @return 转换后的 IR 程序 Ir.ir_program
 *)
val gen_ir : Ast.comp_unit -> Ir.ir_program