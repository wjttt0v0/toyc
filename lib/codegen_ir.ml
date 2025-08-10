(* codegen_ir.ml *)

open Ir

module OpMap = Hashtbl.Make(struct
  type t = operand
  let equal = (=)
  let hash = Hashtbl.hash
end)

(* 代码生成环境 *)
type env = {
  var_map: int OpMap.t;      (* 操作数 -> 栈偏移量 (相对于fp) *)
  stack_size: int;          (* 当前函数总的栈帧大小 *)
  out_channel: out_channel; (* 输出通道 *)
}

(* --- 寄存器别名 --- *)
let sp = "sp"
let fp = "s0" (* Frame Pointer *)
let ra = "ra" (* Return Address *)
let a0 = "a0" (* Argument/Return value *)
let t0 = "t0"
let t1 = "t1"
let t2 = "t2"

(* --- 指令生成辅助函数 --- *)
let emit oc fmt = Printf.fprintf oc (fmt ^^ "\n")
let emit_inst oc inst = emit oc "  %s" inst
let emit_label oc label = emit oc "%s:" label

(* 将操作数加载到指定寄存器 *)
let load_operand env reg op =
  let oc = env.out_channel in
  match op with
  | Const n -> emit_inst oc (Printf.sprintf "li %s, %d" reg n)
  | Var _ | Temp _ ->
      let offset = OpMap.find env.var_map op in
      emit_inst oc (Printf.sprintf "lw %s, %d(%s)" reg offset fp)

(* 将寄存器的值存储到目标操作数（必须是变量或临时变量） *)
let store_operand env reg dest_op =
  match dest_op with
  | Const _ -> failwith "Cannot assign to a constant"
  | Var _ | Temp _ ->
      let offset = OpMap.find env.var_map dest_op in
      emit_inst oc (Printf.sprintf "sw %s, %d(%s)" reg offset fp)

(* === 主生成函数 === *)
let gen_instruction env inst =
  let oc = env.out_channel in
  match inst with
  | Label l -> emit_label oc l
  | Assign (dest, src) ->
      load_operand env t0 src;
      store_operand env t0 dest
  | UnaryOp (dest, op, src) ->
      load_operand env t0 src;
      let op_inst = match op with
        | Ast.Neg -> Printf.sprintf "neg %s, %s" t0 t0
        | Ast.Not -> Printf.sprintf "seqz %s, %s" t0 t0
      in
      emit_inst oc op_inst;
      store_operand env t0 dest
  | BinaryOp (dest, src1, op, src2) ->
      load_operand env t0 src1;
      load_operand env t1 src2;
      let op_inst = match op with
        | Ast.Add -> Printf.sprintf "add %s, %s, %s" t0 t0 t1
        | Ast.Sub -> Printf.sprintf "sub %s, %s, %s" t0 t0 t1
        | Ast.Mul -> Printf.sprintf "mul %s, %s, %s" t0 t0 t1
        | Ast.Div -> Printf.sprintf "div %s, %s, %s" t0 t0 t1
        | Ast.Mod -> Printf.sprintf "rem %s, %s, %s" t0 t0 t1
        | Ast.Eq  -> Printf.sprintf "sub %s, %s, %s; seqz %s, %s" t0 t0 t1 t0 t0
        | Ast.Neq -> Printf.sprintf "sub %s, %s, %s; snez %s, %s" t0 t0 t1 t0 t0
        | Ast.Lt  -> Printf.sprintf "slt %s, %s, %s" t0 t0 t1
        | Ast.Le  -> Printf.sprintf "sgt %s, %s, %s; xori %s, %s, 1" t0 t0 t1 t0 t0
        | Ast.Gt  -> Printf.sprintf "sgt %s, %s, %s" t0 t0 t1
        | Ast.Ge  -> Printf.sprintf "slt %s, %s, %s; xori %s, %s, 1" t0 t0 t1 t0 t0
        | Ast.And | Ast.Or -> failwith "And/Or should have been handled in IR generation"
      in
      emit_inst oc op_inst;
      store_operand env t0 dest
  | GoTo l -> emit_inst oc (Printf.sprintf "j %s" l)
  | IfZero (cond, l) ->
      load_operand env t0 cond;
      emit_inst oc (Printf.sprintf "beqz %s, %s" t0 l)
  | Param op ->
      (* 调用约定：参数从右到左压栈 *)
      load_operand env t0 op;
      emit_inst oc (Printf.sprintf "addi %s, %s, -4" sp sp);
      emit_inst oc (Printf.sprintf "sw %s, 0(%s)" t0 sp)
  | Call (dest_opt, id, args) ->
      emit_inst oc (Printf.sprintf "call %s" id);
      (* 调用者清理栈 *)
      let args_size = 4 * List.length args in
      if args_size > 0 then
        emit_inst oc (Printf.sprintf "addi %s, %s, %d" sp sp args_size);
      (* 保存返回值 *)
      (match dest_opt with
      | Some dest -> store_operand env a0 dest
      | None -> ())
  | Return (Some op) ->
      load_operand env a0 op;
      (* 后续指令会跳转到函数末尾的epilogue *)
  | Return None ->
      ()

let gen_function oc (f: ir_function) =
  emit_label oc f.name;

  (* 1. 计算栈帧大小和变量位置 *)
  let var_map = OpMap.create 16 in
  let offset_counter = ref 0 in
  let add_to_map op =
    if not (OpMap.mem var_map op) then
      begin
        offset_counter := !offset_counter - 4;
        OpMap.add var_map op !offset_counter
      end
  in
  List.iter (fun inst ->
    match inst with
    | Assign (dest, _) | UnaryOp (dest, _, _) | BinaryOp (dest, _, _, _) -> add_to_map dest
    | Call (Some dest, _, _) -> add_to_map dest
    | _ -> ()
  ) f.instrs;

  let locals_size = abs !offset_counter in
  let stack_size = locals_size + 8 (* ra 和 old fp *) in
  let env = { var_map; stack_size; out_channel = oc } in
  
  (* 2. 函数 Prologue *)
  emit_inst oc (Printf.sprintf "addi %s, %s, -%d" sp sp stack_size);
  emit_inst oc (Printf.sprintf "sw %s, %d(%s)" ra (stack_size - 4) sp);
  emit_inst oc (Printf.sprintf "sw %s, %d(%s)" fp (stack_size - 8) sp);
  emit_inst oc (Printf.sprintf "addi %s, %s, %d" fp sp stack_size);
  
  (* TODO: 处理函数参数的传递 (从 a0-a7 或栈) *)
  (* 在这个简单模型里，我们假设参数已在Param指令中被处理 *)

  (* 3. 函数体 *)
  List.iter (gen_instruction env) f.instrs;
  
  (* 4. 函数 Epilogue *)
  (* emit_label oc ("L_exit_" ^ f.name); (* 优化器可能会移除标签,所以这里确保它存在*) *)
  emit_inst oc (Printf.sprintf "addi %s, %s, -%d" sp fp stack_size);
  emit_inst oc (Printf.sprintf "lw %s, %d(%s)" ra (stack_size - 4) sp);
  emit_inst oc (Printf.sprintf "lw %s, %d(%s)" fp (stack_size - 8) sp);
  emit_inst oc (Printf.sprintf "addi %s, %s, %d" sp sp stack_size);
  emit_inst oc "ret"

let gen_program oc (prog: ir_program) =
  emit oc ".text";
  emit oc ".global main";
  List.iter (gen_function oc) prog