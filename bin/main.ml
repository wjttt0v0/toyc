open Lex_project
(* open Ast *)

(* 主函数 *)
let () =
  (* 从标准输入创建词法分析缓冲区 *)
  let lexbuf = Lexing.from_channel stdin in
  try
    (* 解析输入内容生成 AST *)
    let ast = Parser.comp_unit Lexer.token lexbuf in
    (* 进行语义检查 *)
    (* let _ = Semantic.check_comp_unit ast in *)
    (* 输出汇编到标准输出 *)
    Codegen.gen_comp_unit stdout ast
  with
  | Lexer.Error msg -> Printf.eprintf "词法错误: %s\n" msg; exit 1
  | Parser.Error -> Printf.eprintf "语法错误\n"; exit 1
  (* | Semantic.Semantic_error msg -> Printf.eprintf "语义错误: %s\n" msg; exit 1 *)
  | e -> Printf.eprintf "未知错误: %s\n" (Printexc.to_string e); exit 1