
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | WHILE
    | VOID
    | TIMES
    | SEMI
    | RPAREN
    | RETURN
    | RBRACE
    | PLUS
    | OR
    | NUMBER of (
# 6 "lib/parser.mly"
       (int)
# 24 "lib/parser.ml"
  )
    | NOT
    | NEQ
    | MOD
    | MINUS
    | LT
    | LPAREN
    | LE
    | LBRACE
    | INT
    | IF
    | ID of (
# 7 "lib/parser.mly"
       (string)
# 39 "lib/parser.ml"
  )
    | GT
    | GE
    | EQ
    | EOF
    | ELSE
    | DIV
    | CONTINUE
    | COMMA
    | BREAK
    | ASSIGN
    | AND
  
end

include MenhirBasics

# 1 "lib/parser.mly"
  
  open Ast

# 61 "lib/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState000 : ('s, _menhir_box_comp_unit) _menhir_state
    (** State 000.
        Stack shape : .
        Start symbol: comp_unit. *)

  | MenhirState007 : (('s, _menhir_box_comp_unit) _menhir_cell1_func_type _menhir_cell0_ID, _menhir_box_comp_unit) _menhir_state
    (** State 007.
        Stack shape : func_type ID.
        Start symbol: comp_unit. *)

  | MenhirState012 : (('s, _menhir_box_comp_unit) _menhir_cell1_param, _menhir_box_comp_unit) _menhir_state
    (** State 012.
        Stack shape : param.
        Start symbol: comp_unit. *)

  | MenhirState016 : ((('s, _menhir_box_comp_unit) _menhir_cell1_func_type _menhir_cell0_ID, _menhir_box_comp_unit) _menhir_cell1_loption_separated_nonempty_list_COMMA_param__, _menhir_box_comp_unit) _menhir_state
    (** State 016.
        Stack shape : func_type ID loption(separated_nonempty_list(COMMA,param)).
        Start symbol: comp_unit. *)

  | MenhirState018 : (('s, _menhir_box_comp_unit) _menhir_cell1_WHILE, _menhir_box_comp_unit) _menhir_state
    (** State 018.
        Stack shape : WHILE.
        Start symbol: comp_unit. *)

  | MenhirState019 : (('s, _menhir_box_comp_unit) _menhir_cell1_PLUS, _menhir_box_comp_unit) _menhir_state
    (** State 019.
        Stack shape : PLUS.
        Start symbol: comp_unit. *)

  | MenhirState021 : (('s, _menhir_box_comp_unit) _menhir_cell1_NOT, _menhir_box_comp_unit) _menhir_state
    (** State 021.
        Stack shape : NOT.
        Start symbol: comp_unit. *)

  | MenhirState022 : (('s, _menhir_box_comp_unit) _menhir_cell1_MINUS, _menhir_box_comp_unit) _menhir_state
    (** State 022.
        Stack shape : MINUS.
        Start symbol: comp_unit. *)

  | MenhirState023 : (('s, _menhir_box_comp_unit) _menhir_cell1_LPAREN, _menhir_box_comp_unit) _menhir_state
    (** State 023.
        Stack shape : LPAREN.
        Start symbol: comp_unit. *)

  | MenhirState025 : (('s, _menhir_box_comp_unit) _menhir_cell1_ID, _menhir_box_comp_unit) _menhir_state
    (** State 025.
        Stack shape : ID.
        Start symbol: comp_unit. *)

  | MenhirState033 : (('s, _menhir_box_comp_unit) _menhir_cell1_rel_expr _menhir_cell0_rel_op, _menhir_box_comp_unit) _menhir_state
    (** State 033.
        Stack shape : rel_expr rel_op.
        Start symbol: comp_unit. *)

  | MenhirState039 : (('s, _menhir_box_comp_unit) _menhir_cell1_mul_expr _menhir_cell0_mul_op, _menhir_box_comp_unit) _menhir_state
    (** State 039.
        Stack shape : mul_expr mul_op.
        Start symbol: comp_unit. *)

  | MenhirState044 : (('s, _menhir_box_comp_unit) _menhir_cell1_add_expr _menhir_cell0_add_op, _menhir_box_comp_unit) _menhir_state
    (** State 044.
        Stack shape : add_expr add_op.
        Start symbol: comp_unit. *)

  | MenhirState047 : (('s, _menhir_box_comp_unit) _menhir_cell1_or_expr, _menhir_box_comp_unit) _menhir_state
    (** State 047.
        Stack shape : or_expr.
        Start symbol: comp_unit. *)

  | MenhirState051 : (('s, _menhir_box_comp_unit) _menhir_cell1_eq_expr _menhir_cell0_eq_op, _menhir_box_comp_unit) _menhir_state
    (** State 051.
        Stack shape : eq_expr eq_op.
        Start symbol: comp_unit. *)

  | MenhirState055 : (('s, _menhir_box_comp_unit) _menhir_cell1_and_expr, _menhir_box_comp_unit) _menhir_state
    (** State 055.
        Stack shape : and_expr.
        Start symbol: comp_unit. *)

  | MenhirState060 : (('s, _menhir_box_comp_unit) _menhir_cell1_expr, _menhir_box_comp_unit) _menhir_state
    (** State 060.
        Stack shape : expr.
        Start symbol: comp_unit. *)

  | MenhirState069 : ((('s, _menhir_box_comp_unit) _menhir_cell1_WHILE, _menhir_box_comp_unit) _menhir_cell1_expr, _menhir_box_comp_unit) _menhir_state
    (** State 069.
        Stack shape : WHILE expr.
        Start symbol: comp_unit. *)

  | MenhirState071 : (('s, _menhir_box_comp_unit) _menhir_cell1_RETURN, _menhir_box_comp_unit) _menhir_state
    (** State 071.
        Stack shape : RETURN.
        Start symbol: comp_unit. *)

  | MenhirState075 : (('s, _menhir_box_comp_unit) _menhir_cell1_LBRACE, _menhir_box_comp_unit) _menhir_state
    (** State 075.
        Stack shape : LBRACE.
        Start symbol: comp_unit. *)

  | MenhirState078 : (('s, _menhir_box_comp_unit) _menhir_cell1_INT _menhir_cell0_ID, _menhir_box_comp_unit) _menhir_state
    (** State 078.
        Stack shape : INT ID.
        Start symbol: comp_unit. *)

  | MenhirState082 : (('s, _menhir_box_comp_unit) _menhir_cell1_IF, _menhir_box_comp_unit) _menhir_state
    (** State 082.
        Stack shape : IF.
        Start symbol: comp_unit. *)

  | MenhirState084 : ((('s, _menhir_box_comp_unit) _menhir_cell1_IF, _menhir_box_comp_unit) _menhir_cell1_expr, _menhir_box_comp_unit) _menhir_state
    (** State 084.
        Stack shape : IF expr.
        Start symbol: comp_unit. *)

  | MenhirState086 : (('s, _menhir_box_comp_unit) _menhir_cell1_ID, _menhir_box_comp_unit) _menhir_state
    (** State 086.
        Stack shape : ID.
        Start symbol: comp_unit. *)

  | MenhirState094 : (((('s, _menhir_box_comp_unit) _menhir_cell1_IF, _menhir_box_comp_unit) _menhir_cell1_expr, _menhir_box_comp_unit) _menhir_cell1_stmt, _menhir_box_comp_unit) _menhir_state
    (** State 094.
        Stack shape : IF expr stmt.
        Start symbol: comp_unit. *)

  | MenhirState098 : (('s, _menhir_box_comp_unit) _menhir_cell1_stmt, _menhir_box_comp_unit) _menhir_state
    (** State 098.
        Stack shape : stmt.
        Start symbol: comp_unit. *)

  | MenhirState106 : (('s, _menhir_box_comp_unit) _menhir_cell1_func_def, _menhir_box_comp_unit) _menhir_state
    (** State 106.
        Stack shape : func_def.
        Start symbol: comp_unit. *)


and ('s, 'r) _menhir_cell1_add_expr = 
  | MenhirCell1_add_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and 's _menhir_cell0_add_op = 
  | MenhirCell0_add_op of 's * (Ast.binop)

and ('s, 'r) _menhir_cell1_and_expr = 
  | MenhirCell1_and_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and ('s, 'r) _menhir_cell1_eq_expr = 
  | MenhirCell1_eq_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and 's _menhir_cell0_eq_op = 
  | MenhirCell0_eq_op of 's * (Ast.binop)

and ('s, 'r) _menhir_cell1_expr = 
  | MenhirCell1_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and ('s, 'r) _menhir_cell1_func_def = 
  | MenhirCell1_func_def of 's * ('s, 'r) _menhir_state * (Ast.func_def)

and ('s, 'r) _menhir_cell1_func_type = 
  | MenhirCell1_func_type of 's * ('s, 'r) _menhir_state * (Ast.func_type)

and ('s, 'r) _menhir_cell1_loption_separated_nonempty_list_COMMA_param__ = 
  | MenhirCell1_loption_separated_nonempty_list_COMMA_param__ of 's * ('s, 'r) _menhir_state * (Ast.param list)

and ('s, 'r) _menhir_cell1_mul_expr = 
  | MenhirCell1_mul_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and 's _menhir_cell0_mul_op = 
  | MenhirCell0_mul_op of 's * (Ast.binop)

and ('s, 'r) _menhir_cell1_or_expr = 
  | MenhirCell1_or_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and ('s, 'r) _menhir_cell1_param = 
  | MenhirCell1_param of 's * ('s, 'r) _menhir_state * (Ast.param)

and ('s, 'r) _menhir_cell1_rel_expr = 
  | MenhirCell1_rel_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and 's _menhir_cell0_rel_op = 
  | MenhirCell0_rel_op of 's * (Ast.binop)

and ('s, 'r) _menhir_cell1_stmt = 
  | MenhirCell1_stmt of 's * ('s, 'r) _menhir_state * (Ast.stmt)

and ('s, 'r) _menhir_cell1_ID = 
  | MenhirCell1_ID of 's * ('s, 'r) _menhir_state * (
# 7 "lib/parser.mly"
       (string)
# 252 "lib/parser.ml"
)

and 's _menhir_cell0_ID = 
  | MenhirCell0_ID of 's * (
# 7 "lib/parser.mly"
       (string)
# 259 "lib/parser.ml"
)

and ('s, 'r) _menhir_cell1_IF = 
  | MenhirCell1_IF of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_INT = 
  | MenhirCell1_INT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LBRACE = 
  | MenhirCell1_LBRACE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LPAREN = 
  | MenhirCell1_LPAREN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_MINUS = 
  | MenhirCell1_MINUS of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_NOT = 
  | MenhirCell1_NOT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_PLUS = 
  | MenhirCell1_PLUS of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_RETURN = 
  | MenhirCell1_RETURN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_WHILE = 
  | MenhirCell1_WHILE of 's * ('s, 'r) _menhir_state

and _menhir_box_comp_unit = 
  | MenhirBox_comp_unit of (Ast.comp_unit) [@@unboxed]

let _menhir_action_01 =
  fun _1 ->
    (
# 81 "lib/parser.mly"
                                             ( _1 )
# 297 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_02 =
  fun e1 e2 op ->
    (
# 82 "lib/parser.mly"
                                             ( EBinOp(op, e1, e2) )
# 305 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_03 =
  fun () ->
    (
# 101 "lib/parser.mly"
          ( Add )
# 313 "lib/parser.ml"
     : (Ast.binop))

let _menhir_action_04 =
  fun () ->
    (
# 102 "lib/parser.mly"
          ( Sub )
# 321 "lib/parser.ml"
     : (Ast.binop))

let _menhir_action_05 =
  fun _1 ->
    (
# 69 "lib/parser.mly"
                                             ( _1 )
# 329 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_06 =
  fun e1 e2 ->
    (
# 70 "lib/parser.mly"
                                             ( EBinOp(And, e1, e2) )
# 337 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_07 =
  fun stmts ->
    (
# 45 "lib/parser.mly"
                                     ( SBlock stmts )
# 345 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_08 =
  fun f ->
    (
# 30 "lib/parser.mly"
                          ( CUnit f )
# 353 "lib/parser.ml"
     : (Ast.comp_unit))

let _menhir_action_09 =
  fun _1 ->
    (
# 73 "lib/parser.mly"
                                             ( _1 )
# 361 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_10 =
  fun e1 e2 op ->
    (
# 74 "lib/parser.mly"
                                             ( EBinOp(op, e1, e2) )
# 369 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_11 =
  fun () ->
    (
# 110 "lib/parser.mly"
          ( Eq )
# 377 "lib/parser.ml"
     : (Ast.binop))

let _menhir_action_12 =
  fun () ->
    (
# 111 "lib/parser.mly"
          ( Neq )
# 385 "lib/parser.ml"
     : (Ast.binop))

let _menhir_action_13 =
  fun _1 ->
    (
# 62 "lib/parser.mly"
                                             ( _1 )
# 393 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_14 =
  fun b n rt xs ->
    let p = 
# 241 "<standard.mly>"
    ( xs )
# 401 "lib/parser.ml"
     in
    (
# 33 "lib/parser.mly"
                                                                                (
      { return_type = rt; name = n; params = p; body = b }
    )
# 408 "lib/parser.ml"
     : (Ast.func_def))

let _menhir_action_15 =
  fun () ->
    (
# 38 "lib/parser.mly"
         ( TInt )
# 416 "lib/parser.ml"
     : (Ast.func_type))

let _menhir_action_16 =
  fun () ->
    (
# 39 "lib/parser.mly"
         ( TVoid )
# 424 "lib/parser.ml"
     : (Ast.func_type))

let _menhir_action_17 =
  fun () ->
    (
# 216 "<standard.mly>"
    ( [] )
# 432 "lib/parser.ml"
     : (Ast.func_def list))

let _menhir_action_18 =
  fun x xs ->
    (
# 219 "<standard.mly>"
    ( x :: xs )
# 440 "lib/parser.ml"
     : (Ast.func_def list))

let _menhir_action_19 =
  fun () ->
    (
# 216 "<standard.mly>"
    ( [] )
# 448 "lib/parser.ml"
     : (Ast.stmt list))

let _menhir_action_20 =
  fun x xs ->
    (
# 219 "<standard.mly>"
    ( x :: xs )
# 456 "lib/parser.ml"
     : (Ast.stmt list))

let _menhir_action_21 =
  fun () ->
    (
# 145 "<standard.mly>"
    ( [] )
# 464 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_22 =
  fun x ->
    (
# 148 "<standard.mly>"
    ( x )
# 472 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_23 =
  fun () ->
    (
# 145 "<standard.mly>"
    ( [] )
# 480 "lib/parser.ml"
     : (Ast.param list))

let _menhir_action_24 =
  fun x ->
    (
# 148 "<standard.mly>"
    ( x )
# 488 "lib/parser.ml"
     : (Ast.param list))

let _menhir_action_25 =
  fun _1 ->
    (
# 85 "lib/parser.mly"
                                             ( _1 )
# 496 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_26 =
  fun e1 e2 op ->
    (
# 86 "lib/parser.mly"
                                             ( EBinOp(op, e1, e2) )
# 504 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_27 =
  fun () ->
    (
# 105 "lib/parser.mly"
          ( Mul )
# 512 "lib/parser.ml"
     : (Ast.binop))

let _menhir_action_28 =
  fun () ->
    (
# 106 "lib/parser.mly"
          ( Div )
# 520 "lib/parser.ml"
     : (Ast.binop))

let _menhir_action_29 =
  fun () ->
    (
# 107 "lib/parser.mly"
          ( Mod )
# 528 "lib/parser.ml"
     : (Ast.binop))

let _menhir_action_30 =
  fun _1 ->
    (
# 65 "lib/parser.mly"
                                             ( _1 )
# 536 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_31 =
  fun e1 e2 ->
    (
# 66 "lib/parser.mly"
                                             ( EBinOp(Or, e1, e2) )
# 544 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_32 =
  fun i ->
    (
# 42 "lib/parser.mly"
              ( PInt i )
# 552 "lib/parser.ml"
     : (Ast.param))

let _menhir_action_33 =
  fun i ->
    (
# 95 "lib/parser.mly"
                                             ( EInt i )
# 560 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_34 =
  fun i ->
    (
# 96 "lib/parser.mly"
                                             ( EId i )
# 568 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_35 =
  fun e ->
    (
# 97 "lib/parser.mly"
                                             ( e )
# 576 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_36 =
  fun _1 xs ->
    let args = 
# 241 "<standard.mly>"
    ( xs )
# 584 "lib/parser.ml"
     in
    (
# 98 "lib/parser.mly"
                                                         ( ECall(_1, args) )
# 589 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_37 =
  fun _1 ->
    (
# 77 "lib/parser.mly"
                                             ( _1 )
# 597 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_38 =
  fun e1 e2 op ->
    (
# 78 "lib/parser.mly"
                                             ( EBinOp(op, e1, e2) )
# 605 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_39 =
  fun () ->
    (
# 114 "lib/parser.mly"
          ( Lt )
# 613 "lib/parser.ml"
     : (Ast.binop))

let _menhir_action_40 =
  fun () ->
    (
# 115 "lib/parser.mly"
          ( Le )
# 621 "lib/parser.ml"
     : (Ast.binop))

let _menhir_action_41 =
  fun () ->
    (
# 116 "lib/parser.mly"
          ( Gt )
# 629 "lib/parser.ml"
     : (Ast.binop))

let _menhir_action_42 =
  fun () ->
    (
# 117 "lib/parser.mly"
          ( Ge )
# 637 "lib/parser.ml"
     : (Ast.binop))

let _menhir_action_43 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 645 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_44 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 653 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_45 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 661 "lib/parser.ml"
     : (Ast.param list))

let _menhir_action_46 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 669 "lib/parser.ml"
     : (Ast.param list))

let _menhir_action_47 =
  fun () ->
    (
# 48 "lib/parser.mly"
                                             ( SBlock [] )
# 677 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_48 =
  fun expr ->
    (
# 49 "lib/parser.mly"
                                             ( SExpr expr )
# 685 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_49 =
  fun _1 _3 ->
    (
# 50 "lib/parser.mly"
                                             ( SAssign(_1, _3) )
# 693 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_50 =
  fun _2 _4 ->
    (
# 51 "lib/parser.mly"
                                             ( SDecl(_2, _4) )
# 701 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_51 =
  fun cond s1 s2 ->
    (
# 52 "lib/parser.mly"
                                                          ( SIf(cond, s1, Some s2) )
# 709 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_52 =
  fun cond s ->
    (
# 53 "lib/parser.mly"
                                             ( SIf(cond, s, None) )
# 717 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_53 =
  fun cond s ->
    (
# 54 "lib/parser.mly"
                                             ( SWhile(cond, s) )
# 725 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_54 =
  fun () ->
    (
# 55 "lib/parser.mly"
                                             ( SBreak )
# 733 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_55 =
  fun () ->
    (
# 56 "lib/parser.mly"
                                             ( SContinue )
# 741 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_56 =
  fun expr ->
    (
# 57 "lib/parser.mly"
                                             ( SReturn (Some expr) )
# 749 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_57 =
  fun () ->
    (
# 58 "lib/parser.mly"
                                             ( SReturn None )
# 757 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_58 =
  fun stmts ->
    (
# 59 "lib/parser.mly"
                                             ( SBlock stmts )
# 765 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_59 =
  fun _1 ->
    (
# 89 "lib/parser.mly"
                                              ( _1 )
# 773 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_60 =
  fun e ->
    (
# 90 "lib/parser.mly"
                                             ( EUnOp(Neg, e) )
# 781 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_61 =
  fun e ->
    (
# 91 "lib/parser.mly"
                                             ( e )
# 789 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_62 =
  fun e ->
    (
# 92 "lib/parser.mly"
                                             ( EUnOp(Not, e) )
# 797 "lib/parser.ml"
     : (Ast.expr))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | AND ->
        "AND"
    | ASSIGN ->
        "ASSIGN"
    | BREAK ->
        "BREAK"
    | COMMA ->
        "COMMA"
    | CONTINUE ->
        "CONTINUE"
    | DIV ->
        "DIV"
    | ELSE ->
        "ELSE"
    | EOF ->
        "EOF"
    | EQ ->
        "EQ"
    | GE ->
        "GE"
    | GT ->
        "GT"
    | ID _ ->
        "ID"
    | IF ->
        "IF"
    | INT ->
        "INT"
    | LBRACE ->
        "LBRACE"
    | LE ->
        "LE"
    | LPAREN ->
        "LPAREN"
    | LT ->
        "LT"
    | MINUS ->
        "MINUS"
    | MOD ->
        "MOD"
    | NEQ ->
        "NEQ"
    | NOT ->
        "NOT"
    | NUMBER _ ->
        "NUMBER"
    | OR ->
        "OR"
    | PLUS ->
        "PLUS"
    | RBRACE ->
        "RBRACE"
    | RETURN ->
        "RETURN"
    | RPAREN ->
        "RPAREN"
    | SEMI ->
        "SEMI"
    | TIMES ->
        "TIMES"
    | VOID ->
        "VOID"
    | WHILE ->
        "WHILE"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_run_003 : type  ttv_stack. ttv_stack -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _v ->
      let f = _v in
      let _v = _menhir_action_08 f in
      MenhirBox_comp_unit _v
  
  let rec _menhir_run_107 : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_func_def -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _v ->
      let MenhirCell1_func_def (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_18 x xs in
      _menhir_goto_list_func_def_ _menhir_stack _v _menhir_s
  
  and _menhir_goto_list_func_def_ : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _menhir_box_comp_unit =
    fun _menhir_stack _v _menhir_s ->
      match _menhir_s with
      | MenhirState106 ->
          _menhir_run_107 _menhir_stack _v
      | MenhirState000 ->
          _menhir_run_003 _menhir_stack _v
      | _ ->
          _menhir_fail ()
  
  let rec _menhir_run_001 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_16 () in
      _menhir_goto_func_type _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_func_type : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_func_type (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | ID _v_0 ->
          let _menhir_stack = MenhirCell0_ID (_menhir_stack, _v_0) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              let _menhir_s = MenhirState007 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | INT ->
                  _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | RPAREN ->
                  let _v = _menhir_action_23 () in
                  _menhir_goto_loption_separated_nonempty_list_COMMA_param__ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_008 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let i = _v in
          let _v = _menhir_action_32 i in
          (match (_tok : MenhirBasics.token) with
          | COMMA ->
              let _menhir_stack = MenhirCell1_param (_menhir_stack, _menhir_s, _v) in
              let _menhir_s = MenhirState012 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | INT ->
                  _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | RPAREN ->
              let x = _v in
              let _v = _menhir_action_45 x in
              _menhir_goto_separated_nonempty_list_COMMA_param_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_param_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState012 ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState007 ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_013 : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_param -> _ -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_param (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_46 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_param_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_010 : type  ttv_stack. ((ttv_stack, _menhir_box_comp_unit) _menhir_cell1_func_type _menhir_cell0_ID as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_comp_unit) _menhir_state -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let x = _v in
      let _v = _menhir_action_24 x in
      _menhir_goto_loption_separated_nonempty_list_COMMA_param__ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_goto_loption_separated_nonempty_list_COMMA_param__ : type  ttv_stack. ((ttv_stack, _menhir_box_comp_unit) _menhir_cell1_func_type _menhir_cell0_ID as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_comp_unit) _menhir_state -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_loption_separated_nonempty_list_COMMA_param__ (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LBRACE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState016
          | SEMI ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState016
          | RETURN ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState016
          | PLUS ->
              _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState016
          | NUMBER _v_0 ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState016
          | NOT ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState016
          | MINUS ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState016
          | LPAREN ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState016
          | LBRACE ->
              _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState016
          | INT ->
              _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState016
          | IF ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState016
          | ID _v_1 ->
              _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState016
          | CONTINUE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState016
          | BREAK ->
              _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState016
          | RBRACE ->
              let _v_2 = _menhir_action_19 () in
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_017 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_WHILE (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_s = MenhirState018 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | PLUS ->
              _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUMBER _v ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NOT ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_019 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_PLUS (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState019 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUMBER _v ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NOT ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_020 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let i = _v in
      let _v = _menhir_action_33 i in
      _menhir_goto_primary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_primary_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_59 _1 in
      _menhir_goto_unary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_unary_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState019 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState021 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState022 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState039 ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState016 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState069 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState098 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState094 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState071 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState018 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState023 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState060 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState055 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState051 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState044 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_067 : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_PLUS -> _ -> _ -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_PLUS (_menhir_stack, _menhir_s) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_61 e in
      _menhir_goto_unary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_066 : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_NOT -> _ -> _ -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_NOT (_menhir_stack, _menhir_s) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_62 e in
      _menhir_goto_unary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_065 : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_MINUS -> _ -> _ -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_MINUS (_menhir_stack, _menhir_s) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_60 e in
      _menhir_goto_unary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_040 : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_mul_expr _menhir_cell0_mul_op -> _ -> _ -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_mul_op (_menhir_stack, op) = _menhir_stack in
      let MenhirCell1_mul_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_26 e1 e2 op in
      _menhir_goto_mul_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_mul_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState044 ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState016 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState069 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState098 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState094 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState071 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState018 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState023 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState060 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState055 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState051 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_045 : type  ttv_stack. ((ttv_stack, _menhir_box_comp_unit) _menhir_cell1_add_expr _menhir_cell0_add_op as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_comp_unit) _menhir_state -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_mul_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_mul_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_mul_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COMMA | EQ | GE | GT | LE | LT | MINUS | NEQ | OR | PLUS | RPAREN | SEMI ->
          let MenhirCell0_add_op (_menhir_stack, op) = _menhir_stack in
          let MenhirCell1_add_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_02 e1 e2 op in
          _menhir_goto_add_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_036 : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_mul_expr -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_27 () in
      _menhir_goto_mul_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_mul_op : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_mul_expr -> _ -> _ -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_mul_op (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState039
      | NUMBER _v_0 ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState039
      | NOT ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState039
      | MINUS ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState039
      | LPAREN ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState039
      | ID _v_1 ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState039
      | _ ->
          _eRR ()
  
  and _menhir_run_021 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_NOT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState021 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUMBER _v ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NOT ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_022 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_MINUS (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState022 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUMBER _v ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NOT ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_023 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState023 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUMBER _v ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NOT ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_024 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_stack = MenhirCell1_ID (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MOD | NEQ | OR | PLUS | RPAREN | SEMI | TIMES ->
          let i = _v in
          let _v = _menhir_action_34 i in
          _menhir_goto_primary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_025 : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_ID -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState025 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUMBER _v ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NOT ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | RPAREN ->
          let _v = _menhir_action_21 () in
          _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_ID -> _ -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_ID (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_36 _1 xs in
      _menhir_goto_primary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_037 : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_mul_expr -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_29 () in
      _menhir_goto_mul_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_038 : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_mul_expr -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_28 () in
      _menhir_goto_mul_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_add_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState016 ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState018 ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState069 ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState098 ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState094 ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState071 ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState023 ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState060 ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState055 ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState051 ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_053 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_add_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_add_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RPAREN | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_37 _1 in
          _menhir_goto_rel_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_042 : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_add_expr -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_03 () in
      _menhir_goto_add_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_add_op : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_add_expr -> _ -> _ -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_add_op (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState044
      | NUMBER _v_0 ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState044
      | NOT ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState044
      | MINUS ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState044
      | LPAREN ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState044
      | ID _v_1 ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState044
      | _ ->
          _eRR ()
  
  and _menhir_run_043 : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_add_expr -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_04 () in
      _menhir_goto_add_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_rel_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState051 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState016 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState069 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState098 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState094 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState071 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState018 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState023 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState060 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState055 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_052 : type  ttv_stack. ((ttv_stack, _menhir_box_comp_unit) _menhir_cell1_eq_expr _menhir_cell0_eq_op as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_comp_unit) _menhir_state -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LT ->
          let _menhir_stack = MenhirCell1_rel_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_rel_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_rel_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_rel_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COMMA | EQ | NEQ | OR | RPAREN | SEMI ->
          let MenhirCell0_eq_op (_menhir_stack, op) = _menhir_stack in
          let MenhirCell1_eq_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_10 e1 e2 op in
          _menhir_goto_eq_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_029 : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_rel_expr -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_39 () in
      _menhir_goto_rel_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_rel_op : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_rel_expr -> _ -> _ -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_rel_op (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState033
      | NUMBER _v_0 ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState033
      | NOT ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState033
      | MINUS ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState033
      | LPAREN ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState033
      | ID _v_1 ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState033
      | _ ->
          _eRR ()
  
  and _menhir_run_030 : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_rel_expr -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_40 () in
      _menhir_goto_rel_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_031 : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_rel_expr -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_41 () in
      _menhir_goto_rel_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_032 : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_rel_expr -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_42 () in
      _menhir_goto_rel_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_eq_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState055 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState016 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState018 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState069 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState098 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState094 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState071 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState023 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState060 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_056 : type  ttv_stack. ((ttv_stack, _menhir_box_comp_unit) _menhir_cell1_and_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_comp_unit) _menhir_state -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | NEQ ->
          let _menhir_stack = MenhirCell1_eq_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_eq_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COMMA | OR | RPAREN | SEMI ->
          let MenhirCell1_and_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_06 e1 e2 in
          _menhir_goto_and_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_049 : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_eq_expr -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_12 () in
      _menhir_goto_eq_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_eq_op : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_eq_expr -> _ -> _ -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_eq_op (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState051
      | NUMBER _v_0 ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState051
      | NOT ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState051
      | MINUS ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState051
      | LPAREN ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState051
      | ID _v_1 ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState051
      | _ ->
          _eRR ()
  
  and _menhir_run_050 : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_eq_expr -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_11 () in
      _menhir_goto_eq_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_and_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState016 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState018 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState069 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState098 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState094 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState071 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState023 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState060 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_062 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | AND ->
          let _menhir_stack = MenhirCell1_and_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA | OR | RPAREN | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_30 _1 in
          _menhir_goto_or_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_055 : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_and_expr -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState055 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUMBER _v ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NOT ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_or_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OR ->
          let _menhir_stack = MenhirCell1_or_expr (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState047 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | PLUS ->
              _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUMBER _v ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NOT ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | COMMA | RPAREN | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_13 _1 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState016 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState069 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState098 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState094 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState082 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState071 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState018 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState023 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState060 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_096 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let expr = _v in
          let _v = _menhir_action_48 expr in
          _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_stmt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState069 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState016 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState098 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState094 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState084 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_102 : type  ttv_stack. ((ttv_stack, _menhir_box_comp_unit) _menhir_cell1_WHILE, _menhir_box_comp_unit) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _, cond) = _menhir_stack in
      let MenhirCell1_WHILE (_menhir_stack, _menhir_s) = _menhir_stack in
      let s = _v in
      let _v = _menhir_action_53 cond s in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_098 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_stmt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState098
      | SEMI ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState098
      | RETURN ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState098
      | PLUS ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState098
      | NUMBER _v_0 ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState098
      | NOT ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState098
      | MINUS ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState098
      | LPAREN ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState098
      | LBRACE ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState098
      | INT ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState098
      | IF ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState098
      | ID _v_1 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState098
      | CONTINUE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState098
      | BREAK ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState098
      | RBRACE ->
          let _v_2 = _menhir_action_19 () in
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | _ ->
          _eRR ()
  
  and _menhir_run_070 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_47 () in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_071 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_57 () in
          _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_RETURN (_menhir_stack, _menhir_s) in
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | NUMBER _v ->
          let _menhir_stack = MenhirCell1_RETURN (_menhir_stack, _menhir_s) in
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState071
      | NOT ->
          let _menhir_stack = MenhirCell1_RETURN (_menhir_stack, _menhir_s) in
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | MINUS ->
          let _menhir_stack = MenhirCell1_RETURN (_menhir_stack, _menhir_s) in
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | LPAREN ->
          let _menhir_stack = MenhirCell1_RETURN (_menhir_stack, _menhir_s) in
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | ID _v ->
          let _menhir_stack = MenhirCell1_RETURN (_menhir_stack, _menhir_s) in
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState071
      | _ ->
          _eRR ()
  
  and _menhir_run_075 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LBRACE (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | SEMI ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | RETURN ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | PLUS ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | NUMBER _v ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState075
      | NOT ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | MINUS ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | LPAREN ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | LBRACE ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | INT ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | IF ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | ID _v ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState075
      | CONTINUE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | BREAK ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | RBRACE ->
          let _v = _menhir_action_19 () in
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_run_076 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_INT (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ID _v ->
          let _menhir_stack = MenhirCell0_ID (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ASSIGN ->
              let _menhir_s = MenhirState078 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | PLUS ->
                  _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | NUMBER _v ->
                  _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | NOT ->
                  _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MINUS ->
                  _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ID _v ->
                  _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_081 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_IF (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_s = MenhirState082 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | PLUS ->
              _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUMBER _v ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NOT ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_085 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_stack = MenhirCell1_ID (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASSIGN ->
          let _menhir_stack = MenhirCell1_ID (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState086 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | PLUS ->
              _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUMBER _v ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NOT ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | AND | DIV | EQ | GE | GT | LE | LT | MINUS | MOD | NEQ | OR | PLUS | SEMI | TIMES ->
          let i = _v in
          let _v = _menhir_action_34 i in
          _menhir_goto_primary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_089 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_55 () in
          _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_091 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_54 () in
          _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_100 : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_LBRACE -> _ -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LBRACE (_menhir_stack, _menhir_s) = _menhir_stack in
      let stmts = _v in
      let _v = _menhir_action_58 stmts in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_099 : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_stmt -> _ -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_stmt (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_20 x xs in
      _menhir_goto_list_stmt_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_goto_list_stmt_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState016 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState075 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState098 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_103 : type  ttv_stack. ((ttv_stack, _menhir_box_comp_unit) _menhir_cell1_func_type _menhir_cell0_ID, _menhir_box_comp_unit) _menhir_cell1_loption_separated_nonempty_list_COMMA_param__ -> _ -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let stmts = _v in
      let _v = _menhir_action_07 stmts in
      let MenhirCell1_loption_separated_nonempty_list_COMMA_param__ (_menhir_stack, _, xs) = _menhir_stack in
      let MenhirCell0_ID (_menhir_stack, n) = _menhir_stack in
      let MenhirCell1_func_type (_menhir_stack, _menhir_s, rt) = _menhir_stack in
      let b = _v in
      let _v = _menhir_action_14 b n rt xs in
      let _menhir_stack = MenhirCell1_func_def (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VOID ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState106
      | INT ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState106
      | EOF ->
          let _v_0 = _menhir_action_17 () in
          _menhir_run_107 _menhir_stack _v_0
      | _ ->
          _eRR ()
  
  and _menhir_run_002 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_15 () in
      _menhir_goto_func_type _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_095 : type  ttv_stack. (((ttv_stack, _menhir_box_comp_unit) _menhir_cell1_IF, _menhir_box_comp_unit) _menhir_cell1_expr, _menhir_box_comp_unit) _menhir_cell1_stmt -> _ -> _ -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_stmt (_menhir_stack, _, s1) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, cond) = _menhir_stack in
      let MenhirCell1_IF (_menhir_stack, _menhir_s) = _menhir_stack in
      let s2 = _v in
      let _v = _menhir_action_51 cond s1 s2 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_093 : type  ttv_stack. (((ttv_stack, _menhir_box_comp_unit) _menhir_cell1_IF, _menhir_box_comp_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_comp_unit) _menhir_state -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | ELSE ->
          let _menhir_stack = MenhirCell1_stmt (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState094 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEMI ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RETURN ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUMBER _v ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NOT ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACE ->
              _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT ->
              _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | CONTINUE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BREAK ->
              _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | BREAK | CONTINUE | ID _ | IF | INT | LBRACE | LPAREN | MINUS | NOT | NUMBER _ | PLUS | RBRACE | RETURN | SEMI | WHILE ->
          let MenhirCell1_expr (_menhir_stack, _, cond) = _menhir_stack in
          let MenhirCell1_IF (_menhir_stack, _menhir_s) = _menhir_stack in
          let s = _v in
          let _v = _menhir_action_52 cond s in
          _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_087 : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_ID -> _ -> _ -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_ID (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_49 _1 _3 in
          _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_083 : type  ttv_stack. ((ttv_stack, _menhir_box_comp_unit) _menhir_cell1_IF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_comp_unit) _menhir_state -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _menhir_s = MenhirState084 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEMI ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RETURN ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUMBER _v ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NOT ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACE ->
              _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT ->
              _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | CONTINUE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BREAK ->
              _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_079 : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_INT _menhir_cell0_ID -> _ -> _ -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_ID (_menhir_stack, _2) = _menhir_stack in
          let MenhirCell1_INT (_menhir_stack, _menhir_s) = _menhir_stack in
          let _4 = _v in
          let _v = _menhir_action_50 _2 _4 in
          _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_073 : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_RETURN -> _ -> _ -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_RETURN (_menhir_stack, _menhir_s) = _menhir_stack in
          let expr = _v in
          let _v = _menhir_action_56 expr in
          _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_068 : type  ttv_stack. ((ttv_stack, _menhir_box_comp_unit) _menhir_cell1_WHILE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_comp_unit) _menhir_state -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _menhir_s = MenhirState069 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEMI ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RETURN ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUMBER _v ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NOT ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACE ->
              _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT ->
              _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | CONTINUE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BREAK ->
              _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_063 : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_35 e in
          _menhir_goto_primary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_059 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState060 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | PLUS ->
              _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUMBER _v ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NOT ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RPAREN ->
          let x = _v in
          let _v = _menhir_action_43 x in
          _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_expr_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState060 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState025 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_061 : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_expr -> _ -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_expr (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_44 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_027 : type  ttv_stack. (ttv_stack, _menhir_box_comp_unit) _menhir_cell1_ID -> _ -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let x = _v in
      let _v = _menhir_action_22 x in
      _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_stack _menhir_lexbuf _menhir_lexer _v
  
  and _menhir_run_054 : type  ttv_stack. ((ttv_stack, _menhir_box_comp_unit) _menhir_cell1_or_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_comp_unit) _menhir_state -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | AND ->
          let _menhir_stack = MenhirCell1_and_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA | OR | RPAREN | SEMI ->
          let MenhirCell1_or_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_31 e1 e2 in
          _menhir_goto_or_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_048 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | NEQ ->
          let _menhir_stack = MenhirCell1_eq_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_eq_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COMMA | OR | RPAREN | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_05 _1 in
          _menhir_goto_and_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_028 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LT ->
          let _menhir_stack = MenhirCell1_rel_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_rel_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_rel_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_rel_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COMMA | EQ | NEQ | OR | RPAREN | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_09 _1 in
          _menhir_goto_eq_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_041 : type  ttv_stack. ((ttv_stack, _menhir_box_comp_unit) _menhir_cell1_rel_expr _menhir_cell0_rel_op as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_comp_unit) _menhir_state -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_add_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_add_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RPAREN | SEMI ->
          let MenhirCell0_rel_op (_menhir_stack, op) = _menhir_stack in
          let MenhirCell1_rel_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_38 e1 e2 op in
          _menhir_goto_rel_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_035 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_mul_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_mul_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_mul_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COMMA | EQ | GE | GT | LE | LT | MINUS | NEQ | OR | PLUS | RPAREN | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_01 _1 in
          _menhir_goto_add_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_026 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_comp_unit) _menhir_state -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_25 _1 in
      _menhir_goto_mul_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  let _menhir_run_000 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_comp_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VOID ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | INT ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | EOF ->
          let _v = _menhir_action_17 () in
          _menhir_run_003 _menhir_stack _v
      | _ ->
          _eRR ()
  
end

let comp_unit =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_comp_unit v = _menhir_run_000 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
