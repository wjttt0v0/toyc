{
  open Parser (* Opens the Parser module to access token definitions *)
  exception Error of string
}

rule token = parse
  (* Whitespace and Comments: Ignored by recursively calling the lexer *)
  | [' ' '\t' '\n' '\r']      { token lexbuf }
  | "/*"                  { comment lexbuf; token lexbuf }
  | "//"                  { single_line_comment lexbuf; token lexbuf }

  (* Keywords *)
  | "int"       { INT }
  | "void"      { VOID }
  | "if"        { IF }
  | "else"      { ELSE }
  | "while"     { WHILE }
  | "break"     { BREAK }
  | "continue"  { CONTINUE }
  | "return"    { RETURN }

  (* Operators and Delimiters *)
  | "("         { LPAREN }
  | ")"         { RPAREN }
  | "{"         { LBRACE }
  | "}"         { RBRACE }
  | ";"         { SEMI }
  | ","         { COMMA }
  | "="         { ASSIGN }
  | "=="        { EQ }
  | "!="        { NEQ }
  | "<"         { LT }
  | "<="        { LE }
  | ">"         { GT }
  | ">="        { GE }
  | "&&"        { AND }
  | "||"        { OR }
  | "!"         { NOT }
  | "+"         { PLUS }
  | "-"         { MINUS }
  | "*"         { TIMES }
  | "/"         { DIV }
  | "%"         { MOD }

    (* Identifiers and Numbers *)
  | ['_''A'-'Z''a'-'z']['_''A'-'Z''a'-'z''0'-'9']* as lxm { ID lxm }
  | ('0' | ['1'-'9']['0'-'9']*) as lxm { NUMBER (int_of_string lxm) }

  (* End of File *)
  | eof { EOF }
  | _ as c { raise (Error ("非法字符: " ^ String.make 1 c)) }

and comment = parse
  | "*/" { () }
  | _    { comment lexbuf }

and single_line_comment = parse
  | ['\n' '\r'] { () }
  | _    { single_line_comment lexbuf }