open Lex_project
(* open Ast *)

(* 从标准输入读取所有内容的函数 *)
let read_stdin_all () =
  let buf = Buffer.create 1024 in
  try
    while true do
      let line = input_line stdin in
      Buffer.add_string buf line;
      Buffer.add_char buf '\n' (* input_line 会去掉换行符，我们把它加回来 *)
    done;
    "" (* 永不执行，仅为类型检查 *)
  with End_of_file ->
    Buffer.contents buf

(* 主函数 *)
let () =
  (* 从标准输入读取内容 *)
  let content = read_stdin_all () in
  (* 创建词法分析缓冲区 *)
  let lexbuf = Lexing.from_string content in
  try
    (* 解析内容生成 AST *)
    let ast = Parser.comp_unit Lexer.token lexbuf in
    (* 进行语义检查 *)
    let _ = Semantic.check_comp_unit ast in
    (* 生成 RISC-V 汇编代码到标准输出 *)
    Codegen.gen_comp_unit stdout ast;
    (* 刷新标准输出缓冲区，确保所有内容都被写出 *)
    flush stdout
  with
  | Lexer.Error msg -> Printf.eprintf "词法错误: %s\n" msg; exit 1
  | Parser.Error -> Printf.eprintf "语法错误\n"; exit 1
  | Semantic.Semantic_error msg -> Printf.eprintf "语义错误: %s\n" msg; exit 1
  | e -> Printf.eprintf "未知错误: %s\n" (Printexc.to_string e); exit 1