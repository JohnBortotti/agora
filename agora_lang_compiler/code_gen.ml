open Ast

let opcode_of_binop = function
  | Add -> "3"
  | Sub -> "4"
  | Mul -> "5"
  | Div -> "6"
  | Eq -> "10"
  | Neq -> "11"
  | Lt -> "12"
  | Lte -> "14"
  | Gt -> "11"
  | Gte -> "15"

let emit_opcode buf opcode =
  let hex_opcode = Printf.sprintf "%02x" opcode in
  Buffer.add_string buf hex_opcode

let emit_u256 buf value =
  let hex_value = Printf.sprintf "%064x" value in
  Buffer.add_string buf hex_value

let emit_string buf bytes_len s =
  let length = String.length s in
  let hex_length = Printf.sprintf "%0*x" (bytes_len * 2) length in
  let hex_data = String.concat "" (List.map (fun c -> Printf.sprintf "%02x" (Char.code c)) (String.to_seq s |> List.of_seq)) in
  Buffer.add_string buf (hex_length ^ hex_data)

let rec compile_expr buf = function
  | Int n ->
      emit_opcode buf 0x01;
      emit_u256 buf n;
  | String s ->
      emit_opcode buf 0x01;
      emit_string buf 2 s
  | BinOp (op, lhs, rhs) ->
      compile_expr buf lhs;
      compile_expr buf rhs;
      let opcode = match op with
        | Add -> 0x03
        | Sub -> 0x04
        | Mul -> 0x05
        | Div -> 0x06
        | Eq -> 0x10
        | Neq -> 0x11
        | Lt -> 0x12
        | Gt -> 0x13
        | Lte -> 0x14
        | Gte -> 0x15
      in
      emit_opcode buf opcode
  | Assign (var, value) ->
      compile_expr buf value;
      emit_opcode buf 0x30;
      compile_expr buf var;
  | VarBind (name, _t, value) ->
      compile_expr buf value;
      emit_opcode buf 0x30;
      emit_string buf 2 name
  | e -> failwith (Printf.sprintf "Not implemented code generation: %s" (string_of_expr e))
