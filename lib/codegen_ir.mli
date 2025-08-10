(* codegen_ir.mli *)

(**
 * IR 到 RISC-V 汇编代码的生成器
 *)

(**
 * 将一个 IR 程序转换为汇编代码，并写入输出通道。
 * @param oc 输出通道 (e.g., stdout)
 * @param prog 要转换的 IR 程序
 *)
val gen_program : out_channel -> Ir.ir_program -> unit