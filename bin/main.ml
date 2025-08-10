(* bin/main.ml *)
open Lex_project

let () =
  let lexbuf = Lexing.from_channel stdin in
  try
    (* 1. 解析 -> AST *)
    let ast = Parser.comp_unit Lexer.token lexbuf in

    (* 可插入语义检查 *)
    (* let () = Semantic.check_comp_unit ast in *)
    
    (* 2. AST -> IR *)
    let ir = Ir_gen.gen_ir ast in

    (* (用于调试) 打印原始 IR
    print_endline "--- Original IR ---";
    Ir.print_ir_program stdout ir;
    print_endline "------------------";
    flush stdout;
    *)

    (* 3. IR 优化 *)
    let optimized_ir = Ir_optimizer.optimize ir in
    
    (* (用于调试) 打印优化后的 IR
    print_endline "--- Optimized IR ---";
    Ir.print_ir_program stdout optimized_ir;
    print_endline "--------------------";
    flush stdout;
    *)

    (* 4. Optimized IR -> Assembly *)
    Codegen_ir.gen_program stdout optimized_ir

  with
  | Lexer.Error msg -> Printf.eprintf "[Lexer Error] %s\n" msg; exit 1
  | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      Printf.eprintf "[Parser Error] Syntax error at line %d, column %d.\n"
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
      exit 1
  | Failure msg -> Printf.eprintf "[Runtime Failure] %s\n" msg; exit 1
  | e -> Printf.eprintf "[Unknown Error] %s\n" (Printexc.to_string e); exit 1