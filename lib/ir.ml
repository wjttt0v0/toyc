(* ir.ml *)
open Ast

(* 操作数：变量、临时变量或常量 *)
type operand =
  | Var of id       (* 源程序中的变量，如 "x" *)
  | Temp of int     (* 临时变量，如 t1, t2 *)
  | Const of int    (* 常量，如 100 *)

(* 三地址码指令 *)
type instruction =
  | Label of string                     (* 标签，如 L1: *)
  | Assign of operand * operand         (* dest := src *)
  | UnaryOp of operand * unop * operand (* dest := op src *)
  | BinaryOp of operand * operand * binop * operand (* dest := src1 op src2 *)
  | GoTo of string                      (* goto L1 *)
  | IfZero of operand * string          (* if x == 0 goto L1 *)
  | Param of operand                    (* 将一个变量标记为函数参数 *)
  | Call of operand option * id * operand list (* (dest) := call func(args) *)
  | Return of operand option            (* return (val) *)

(* 函数的 IR 表示 *)
type ir_function = {
  name: id;
  params: operand list;
  instrs: instruction list;
}

(* 整个程序的 IR 表示 *)
type ir_program = ir_function list

(*** 以下是用于调试的打印函数 ***)

let string_of_operand = function
  | Var id -> id
  | Temp i -> "t" ^ string_of_int i
  | Const i -> string_of_int i

let string_of_unop = function
  | Neg -> "-"
  | Not -> "!"

let string_of_binop = function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%"
  | Eq -> "==" | Neq -> "!=" | Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">="
  | And -> "&&" | Or -> "||"

let print_instruction oc inst =
  let p fmt = Printf.fprintf oc fmt in
  match inst with
  | Label l -> p "%s:\n" l
  | Assign (dest, src) ->
      p "  %s := %s\n" (string_of_operand dest) (string_of_operand src)
  | UnaryOp (dest, op, src) ->
      p "  %s := %s %s\n" (string_of_operand dest) (string_of_unop op) (string_of_operand src)
  | BinaryOp (dest, src1, op, src2) ->
      p "  %s := %s %s %s\n" (string_of_operand dest) (string_of_operand src1) (string_of_binop op) (string_of_operand src2)
  | GoTo l -> p "  goto %s\n" l
  | IfZero (cond, l) ->
      p "  if %s == 0 goto %s\n" (string_of_operand cond) l
  | Param p_op -> p "  param %s\n" (string_of_operand p_op)
  | Call (Some dest, id, args) ->
      p "  %s := call %s(%s)\n" (string_of_operand dest) id (String.concat ", " (List.map string_of_operand args))
  | Call (None, id, args) ->
      p "  call %s(%s)\n" id (String.concat ", " (List.map string_of_operand args))
  | Return (Some op) -> p "  return %s\n" (string_of_operand op)
  | Return None -> p "  return\n"

let print_ir_function oc f =
  Printf.fprintf oc "function %s:\n" f.name;
  List.iter (print_instruction oc) f.instrs;
  Printf.fprintf oc "\n"

let print_ir_program oc prog =
  List.iter (print_ir_function oc) prog