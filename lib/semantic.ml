(* semantic.ml *)
open Ast

(* 类型定义 *)
type ty = TInt | TBool | TVoid | TFunc of ty * ty list

(* 符号表：嵌套作用域，每个作用域是一个 (string * ty) list *)
type env = (string * ty) list list

(* 语义错误异常 *)
exception Semantic_error of string

(* 将 func_type 转换为 ty *)
let convert_func_type = function
  | Ast.TInt -> TInt
  | Ast.TVoid -> TVoid

(* 查找符号表，从内层到外层 *)
let rec lookup env id =
  match env with
  | [] -> raise (Semantic_error ("未声明的标识符: " ^ id))
  | scope :: rest ->
      begin try
        List.assoc id scope
      with Not_found -> lookup rest id
      end

(* 添加变量到当前作用域，检查重复定义 *)
let add_to_scope scope id ty =
  if List.exists (fun (name, _) -> name = id) scope then
    raise (Semantic_error ("重复定义: " ^ id))
  else
    (id, ty) :: scope

(* 添加到全局作用域（符号表的第一层） *)
let add_to_global env id ty =
  match env with
  | [] -> [[(id, ty)]]
  | global_scope :: rest -> add_to_scope global_scope id ty :: rest

(* 添加到当前（最内层）作用域 *)
let add env id ty =
  match env with
  | [] -> [[(id, ty)]]
  | scope :: rest -> add_to_scope scope id ty :: rest

(* 创建新作用域 *)
let enter_scope env = [] :: env

(* 退出作用域 *)
let exit_scope env =
  match env with
  | [] -> failwith "无法退出空作用域"
  | _ :: rest -> rest

(* 检查表达式类型 *)
let rec check_expr env = function
  | EInt _ -> TInt
  | EId id -> lookup env id
  | EUnOp (op, e) ->
      let t = check_expr env e in
      begin match op, t with
      | Neg, TInt -> TInt
      | Not, TInt -> TBool
      | _ -> raise (Semantic_error "一元运算类型错误")
      end
  | EBinOp (op, e1, e2) ->
      let t1 = check_expr env e1 in
      let t2 = check_expr env e2 in
      begin match op, t1, t2 with
      | (Add | Sub | Mul | Div | Mod), TInt, TInt -> TInt
      | (Eq | Neq | Lt | Le | Gt | Ge), TInt, TInt -> TBool
      | (And | Or), TInt, TInt -> TBool
      | _ -> raise (Semantic_error "二元运算类型错误")
      end
  | ECall (id, args) ->
      let func_ty = lookup env id in
      begin match func_ty with
      | TFunc (ret_ty, param_tys) ->
          let arg_tys = List.map (check_expr env) args in
          if arg_tys = param_tys then ret_ty
          else raise (Semantic_error ("函数 " ^ id ^ " 参数类型不匹配"))
      | _ -> raise (Semantic_error (id ^ " 不是函数"))
      end

(* 检查语句，返回更新后的符号表 *)
let rec check_stmt env ret_ty = function
  | SExpr e ->
      ignore (check_expr env e);
      env
  | SBlock stmts ->
      let block_env = enter_scope env in
      let final_env = List.fold_left (fun env stmt -> check_stmt env ret_ty stmt) block_env stmts in
      exit_scope final_env
  | SIf (cond, s1, s2_opt) ->
      if check_expr env cond = TBool || check_expr env cond = TInt then
        let env1 = check_stmt env ret_ty s1 in
        match s2_opt with
        | Some s2 -> check_stmt env1 ret_ty s2
        | None -> env1
      else
        raise (Semantic_error "if 条件必须是布尔类型")
  | SWhile (cond, s) ->
      if check_expr env cond = TBool || check_expr env cond = TInt then
        check_stmt env ret_ty s
      else
        raise (Semantic_error "while 条件必须是布尔类型")
  | SBreak | SContinue -> env
  | SReturn None ->
      if ret_ty = TVoid then env
      else raise (Semantic_error "返回类型不匹配: 期望 void")
  | SReturn (Some e) ->
      let t = check_expr env e in
      if t = ret_ty then env
      else raise (Semantic_error "返回类型不匹配")
  | SDecl (id, e) ->
      let t = check_expr env e in
      if t = TInt then
        add env id TInt
      else
        raise (Semantic_error "变量声明必须是 int 类型")
  | SAssign (id, e) ->
      let var_ty = lookup env id in
      let expr_ty = check_expr env e in
      if var_ty = TInt && expr_ty = TInt then env
      else raise (Semantic_error "赋值类型不匹配")

(* 检查函数定义，返回更新后的全局符号表 *)
let check_func env f =
  (* 在全局符号表中添加函数名，支持递归调用 *)
  let func_ty = TFunc (convert_func_type f.return_type, List.map (fun _ -> TInt) f.params) in
  let global_env = add_to_global env f.name func_ty in
  (* 创建函数作用域，包含参数 *)
  let func_env = enter_scope global_env in
  let param_env = List.fold_left (fun env p -> match p with PInt id -> add env id TInt) func_env f.params in
  (* 检查函数体 *)
  let _ = check_stmt param_env (convert_func_type f.return_type) f.body in
  (* 返回全局符号表 *)
  global_env

(* 检查编译单元 *)
let check_comp_unit (CUnit funcs) =
  let global_env = List.fold_left (fun env f -> check_func env f) [[]] funcs in
  (* 确保存在 main 函数 *)
  match lookup global_env "main" with
  | TFunc (ret_ty, []) -> if ret_ty = TInt then global_env else raise (Semantic_error "main 必须返回 int")
  | _ -> raise (Semantic_error "缺少有效的 main 函数")