(* ir_optimizer.mli *)

(**
 * 中间表示 (IR) 优化器
 *)

(**
 * 对整个 IR 程序应用优化。
 * @param prog 未经优化的 IR 程序
 * @return 优化后的 IR 程序
 *)
val optimize : Ir.ir_program -> Ir.ir_program