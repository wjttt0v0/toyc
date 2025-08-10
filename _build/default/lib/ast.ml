(* ast.ml *)
type id = string

type unop = Not | Neg

type binop =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Le | Gt | Ge
  | And | Or

type expr =
  | EInt of int
  | EId of id
  | EUnOp of unop * expr
  | EBinOp of binop * expr * expr
  | ECall of id * expr list

type stmt =
  | SExpr of expr
  | SBlock of stmt list
  | SIf of expr * stmt * stmt option
  | SWhile of expr * stmt
  | SReturn of expr option
  | SBreak
  | SContinue
  | SDecl of id * expr
  | SAssign of id * expr

type param = PInt of id
type func_type = TInt | TVoid

type func_def = {
  return_type: func_type;
  name: id;
  params: param list;
  body: stmt;
}

type comp_unit = CUnit of func_def list

(* 打印一元操作符 *)
let string_of_unop = function
  | Not -> "Not"
  | Neg -> "Neg"
  
(* 打印二元操作符 *)
let string_of_binop = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Mod -> "Mod"
  | Eq -> "Eq"
  | Neq -> "Neq"
  | Lt -> "Lt"
  | Le -> "Le"
  | Gt -> "Gt"
  | Ge -> "Ge"
  | And -> "And"
  | Or -> "Or"

(* 递归打印表达式 *)
let rec print_expr = function
  | EInt i -> Printf.printf "EInt %d" i
  | EId id -> Printf.printf "EId %s" id
  | EUnOp (op, e) ->
      Printf.printf "EUnOp (%s, " (string_of_unop op);
      print_expr e;
      Printf.printf ")"
  | EBinOp (op, e1, e2) ->
      Printf.printf "EBinOp (%s, " (string_of_binop op);
      print_expr e1;
      Printf.printf ", ";
      print_expr e2;
      Printf.printf ")"
  | ECall (id, args) ->
      Printf.printf "ECall (%s, [" id;
      List.iter (fun e -> print_expr e; Printf.printf "; ") args;
      Printf.printf "])"

let print_expr_option = function
  | Some e -> print_expr e
  | None -> Printf.printf "None"
(* 打印语句 *)
let rec print_stmt = function
  | SExpr e ->
      Printf.printf "SExpr (";
      print_expr e;
      Printf.printf ")\n"
  | SReturn e_opt ->
      Printf.printf "SReturn (";
      print_expr_option e_opt;
      Printf.printf ")\n"
  | SBlock stmts ->
      Printf.printf "SBlock [\n";
      List.iter print_stmt stmts;
      Printf.printf "]\n"
  | SIf (cond, then_stmt, else_stmt_opt) ->
      Printf.printf "SIf (";
      print_expr cond;
      Printf.printf ", ";
      print_stmt then_stmt;
      (match else_stmt_opt with
       | Some else_stmt ->
           Printf.printf ", ";
           print_stmt else_stmt
       | None -> ());
      Printf.printf ")\n"
  | SWhile (cond, body) ->
      Printf.printf "SWhile (";
      print_expr cond;
      Printf.printf ", ";
      print_stmt body;
      Printf.printf ")\n"
  | SBreak ->
      Printf.printf "SBreak\n"
  | SContinue ->
      Printf.printf "SContinue\n"
  | SDecl (id, expr) ->
      Printf.printf "SDecl (%s, " id;
      print_expr expr;
      Printf.printf ")\n"
  | SAssign(id, expr) ->
      Printf.printf "SAssign (%s, " id;
      print_expr expr;
      Printf.printf ")\n"

(* 打印函数定义 *)
let print_func_def func =
  Printf.printf "FuncDef (%s, [" func.name;
  List.iter (fun p -> match p with PInt id -> Printf.printf "%s; " id) func.params;
  Printf.printf "],\n";
  print_stmt func.body;
  Printf.printf ")\n"

(* 打印编译单元 *)
let print_comp_unit (CUnit funcs) =
  Printf.printf "CompUnit [\n";
  List.iter print_func_def funcs;
  Printf.printf "]\n"