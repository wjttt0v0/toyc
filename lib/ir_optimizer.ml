(* ir_optimizer.ml *)

open Ir

module OperandSet = Set.Make(struct
  type t = operand
  let compare = compare
end)

(* === Pass 1: 死代码消除 === *)
(*
 * 1. 消除不可达代码 (在无条件跳转之后，下一个标签之前)。
 * 2. 消除对从未被读取的临时变量的赋值。
 *)
let eliminate_dead_code (instrs: instruction list) : instruction list =
  (* 步骤 1: 找出所有被读取过的操作数 *)
  let used_operands =
    List.fold_left (fun acc inst ->
      match inst with
      | Assign (_, src) | UnaryOp (_, _, src) | Param src | IfZero (src, _) -> OperandSet.add src acc
      | BinaryOp (_, src1, _, src2) -> Operand_set.add src1 (Operand_set.add src2 acc)
      | Call (_, _, args) -> List.fold_left (fun s a -> OperandSet.add a s) acc args
      | Return (Some op) -> Operand_set.add op acc
      | _ -> acc
    ) OperandSet.empty instrs
  in

  (* 步骤 2: 遍历指令，移除死赋值和不可达代码 *)
  let rec process instrs_in reachable =
    match instrs_in with
    | [] -> []
    | inst :: rest ->
        if not reachable then
          (* 如果当前代码不可达，则跳过直到遇到标签 *)
          match inst with
          | Label _ -> process instrs_in true (* 代码再次变为可达 *)
          | _ -> process rest false
        else
          (* 当前代码是可达的 *)
          let next_reachable = match inst with
            | GoTo _ | Return _ -> false (* 此指令之后变为不可达 *)
            | _ -> true
          in
          let is_dead_assignment = match inst with
            | Assign (Temp t, _) | UnaryOp (Temp t, _, _) | BinaryOp (Temp t, _, _, _) ->
                not (OperandSet.mem (Temp t) used_operands)
            | Call (Some (Temp t), _, _) ->
                not (OperandSet.mem (Temp t) used_operands)
            | _ -> false
          in
          if is_dead_assignment then
            process rest next_reachable (* 丢弃这条死指令 *)
          else
            inst :: process rest next_reachable (* 保留这条指令 *)
  in
  process instrs true (* 初始状态是可达的 *)

(* === Pass 2: 常量传播与折叠 === *)
let constant_propagation (instrs: instruction list) : instruction list =
  let const_map = Hashtbl.create 16 in (* (operand -> int) *)

  let get_val op =
    if Hashtbl.mem const_map op then Some (Hashtbl.find const_map op)
    else match op with | Const n -> Some n | _ -> None
  in

  List.map (fun inst ->
    (* 尝试用常量替换源操作数 *)
    let map_op op = if Hashtbl.mem const_map op then Const (Hashtbl.find const_map op) else op in
    
    match inst with
    | Assign (dest, src) ->
        let src' = map_op src in
        (match get_val src' with
         | Some n -> Hashtbl.replace const_map dest n; Assign (dest, Const n)
         | None -> Hashtbl.remove const_map dest; Assign (dest, src'))
    
    | UnaryOp (dest, op, src) ->
        let src' = map_op src in
        (match (op, get_val src') with
         | (Ast.Neg, Some n) -> let v = -n in Hashtbl.replace const_map dest v; Assign(dest, Const v)
         | (Ast.Not, Some n) -> let v = if n = 0 then 1 else 0 in Hashtbl.replace const_map dest v; Assign(dest, Const v)
         | _ -> Hashtbl.remove const_map dest; UnaryOp(dest, op, src'))

    | BinaryOp (dest, src1, op, src2) ->
        let src1' = map_op src1 in
        let src2' = map_op src2 in
        (match (op, get_val src1', get_val src2') with
         | (Ast.Add, Some n1, Some n2) -> let v=n1+n2 in Hashtbl.replace const_map dest v; Assign(dest, Const v)
         | (Ast.Sub, Some n1, Some n2) -> let v=n1-n2 in Hashtbl.replace const_map dest v; Assign(dest, Const v)
         | (Ast.Mul, Some n1, Some n2) -> let v=n1*n2 in Hashtbl.replace const_map dest v; Assign(dest, Const v)
         (* ... 可添加更多操作的折叠 ... *)
         | _ -> Hashtbl.remove const_map dest; BinaryOp(dest, src1', op, src2'))
    
    | IfZero(op, label) ->
        (match get_val op with
        | Some 0 -> GoTo label (* 总是跳转 *)
        | Some _ -> inst (* 永远不跳转, 但我们保留原指令，死代码消除会处理下一条goto *)
        | None -> inst) (* 未知，保持原样 *)

    | _ -> inst (* 其他指令不受影响 *)
  ) instrs


(* 主优化函数，重复应用优化遍直到 IR 不再改变 (达到不动点) *)
let optimize_function (f: ir_function) : ir_function =
  let rec fixed_point current_instrs =
    let after_const_prop = constant_propagation current_instrs in
    let after_dead_code = eliminate_dead_code after_const_prop in
    
    if after_dead_code = current_instrs then
      current_instrs (* 不动点，停止优化 *)
    else
      fixed_point after_dead_code (* 继续优化 *)
  in
  { f with instrs = fixed_point f.instrs }


let optimize (prog: ir_program) : ir_program =
  List.map optimize_function prog