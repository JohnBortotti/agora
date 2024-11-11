open Ast

let opcodes = [
  ("PUSH", 0x01);
  ("POP", 0x02);
  ("ADD", 0x03);
  ("SUB", 0x04);
  ("MUL", 0x05);
  ("DIV", 0x06);
  ("MOD", 0x07);
  ("EQ", 0x10);
  ("NE", 0x11);
  ("LT", 0x12);
  ("GT", 0x13);
  ("LE", 0x14);
  ("GE", 0x15);
  ("AND", 0x16);
  ("OR", 0x17);
  ("BAND", 0x20);
  ("BOR", 0x21);
  ("BXOR", 0x22);
  ("BNOT", 0x23);
  ("SHL", 0x24);
  ("SHR", 0x25);
  ("SET", 0x30);
  ("GET", 0x31);
  ("JUMP", 0x40);
  ("JUMPI", 0x41);
  ("HALT", 0x42);
  ("SHA256", 0x43);
  ("EMIT", 0x70);
  ("RETURN", 0x52);
]

let emit_opcode buf opcode =
  Buffer.add_char buf (Char.chr opcode)

let emit_u256 buf value =
  for i = 0 to 31 do
    let byte = (value lsr (8 * (31 - i))) land 0xff in
    Buffer.add_char buf (Char.chr byte)
  done

let string_to_u256_chunks s =
  let len = String.length s in
  let num_chunks = (len + 31) / 32 in
  let chunks = ref [] in
  
  for i = 0 to num_chunks - 1 do
    let chunk_value = ref 0 in
    for j = 0 to 31 do
      let pos = (i * 32) + j in
      if pos < len then
        chunk_value := (!chunk_value lsl 8) + Char.code s.[pos]
      else
        chunk_value := !chunk_value lsl 8
    done;
    chunks := !chunk_value :: !chunks
  done;
  List.rev !chunks

let compile_string_hash buf s =
  let chunks = string_to_u256_chunks s in
  List.iter (fun chunk ->
    emit_opcode buf (List.assoc "PUSH" opcodes);
    emit_u256 buf chunk
  ) chunks;
  emit_opcode buf (List.assoc "SHA256" opcodes);
  emit_u256 buf (List.length chunks);
  List.length chunks + 1 (* Return number of instructions generated *)

type storage_var = 
  | StateVar
  | Mapping
  | Stack
  | Function of int
  | Event of ty list

type compilation_context = {
  symbols: (string, storage_var) Hashtbl.t;
  mutable current_offset: int;
}

let create_context () = {
  symbols = Hashtbl.create 64;
  current_offset = 0;
}

let rec compile_expr ctx buf = function
  | Int n ->
      emit_opcode buf (List.assoc "PUSH" opcodes);
      emit_u256 buf n;
      ctx.current_offset <- ctx.current_offset + 1

  | Bool b ->
      emit_opcode buf (List.assoc "PUSH" opcodes);
      emit_u256 buf (if b then 1 else 0);
      ctx.current_offset <- ctx.current_offset + 1

  | String s ->
      let instr_count = compile_string_hash buf s in
      ctx.current_offset <- ctx.current_offset + instr_count

  | BinOp (op, e1, e2) ->
      compile_expr ctx buf e1;
      compile_expr ctx buf e2;
      let opcode = match op with
        | Add -> List.assoc "ADD" opcodes
        | Sub -> List.assoc "SUB" opcodes
        | Mul -> List.assoc "MUL" opcodes
        | Div -> List.assoc "DIV" opcodes
        | Eq -> List.assoc "EQ" opcodes
        | Neq -> List.assoc "NE" opcodes
        | Lt -> List.assoc "LT" opcodes
        | Gt -> List.assoc "GT" opcodes
        | Lte -> List.assoc "LE" opcodes
        | Gte -> List.assoc "GE" opcodes
      in
      emit_opcode buf opcode;
      ctx.current_offset <- ctx.current_offset + 1

  | Var name ->
      (match Hashtbl.find_opt ctx.symbols name with
      | Some StateVar | Some Mapping ->
          let instr_count = compile_string_hash buf name in
          emit_opcode buf (List.assoc "GET" opcodes);
          ctx.current_offset <- ctx.current_offset + instr_count + 1
      | Some Stack -> ()
      | Some (Function offset) ->
          emit_opcode buf (List.assoc "PUSH" opcodes);
          emit_u256 buf offset;
          ctx.current_offset <- ctx.current_offset + 1
      | _ -> failwith (Printf.sprintf "Unknown variable: %s" name))

  | VarBind (name, _ty, value) -> 
      Hashtbl.add ctx.symbols name StateVar;
      compile_expr ctx buf value;
      let instr_count = compile_string_hash buf name in
      emit_opcode buf (List.assoc "SET" opcodes);
      ctx.current_offset <- ctx.current_offset + instr_count + 1

  | Mapping (name, _, _) ->
      Hashtbl.add ctx.symbols name Mapping

  | IndexAccess (Var map_name, key) ->
      (match Hashtbl.find_opt ctx.symbols map_name with
      | Some Mapping ->
          compile_expr ctx buf key;
          let instr_count = compile_string_hash buf map_name in
          emit_opcode buf (List.assoc "SHA256" opcodes);
          emit_u256 buf 2;
          emit_opcode buf (List.assoc "GET" opcodes);
          ctx.current_offset <- ctx.current_offset + instr_count + 2
      | _ -> failwith "Not a mapping")

  | Assign (lhs, rhs) ->
      compile_expr ctx buf rhs;
      (match lhs with
      | IndexAccess (Var map_name, key) ->
          (match Hashtbl.find_opt ctx.symbols map_name with
          | Some Mapping ->
              compile_expr ctx buf key;
              let instr_count = compile_string_hash buf map_name in
              emit_opcode buf (List.assoc "SHA256" opcodes);
              emit_u256 buf 2;
              emit_opcode buf (List.assoc "SET" opcodes);
              ctx.current_offset <- ctx.current_offset + instr_count + 2
          | _ -> failwith "Not a mapping")
      | Var name ->
          (match Hashtbl.find_opt ctx.symbols name with
          | Some StateVar ->
              let instr_count = compile_string_hash buf name in
              emit_opcode buf (List.assoc "SET" opcodes);
              ctx.current_offset <- ctx.current_offset + instr_count + 1
          | _ -> failwith "Cannot assign to non-storage variable")
      | _ -> failwith "Invalid assignment target")

  | Let (name, value, body) ->
      compile_expr ctx buf value;
      Hashtbl.add ctx.symbols name Stack;
      compile_expr ctx buf body;
      Hashtbl.remove ctx.symbols name

  | Let_brace (name, body) ->
      let func_offset = ctx.current_offset in
      Hashtbl.add ctx.symbols name (Function func_offset);
      List.iter (compile_expr ctx buf) body;
      emit_opcode buf (List.assoc "RETURN" opcodes);
      ctx.current_offset <- ctx.current_offset + 1

  | If (cond, then_branch, else_branch) ->
    compile_expr ctx buf cond;
    let else_pos = ctx.current_offset in
    emit_opcode buf (List.assoc "JUMPI" opcodes);
    emit_u256 buf 0;  (* placeholder *)
    ctx.current_offset <- ctx.current_offset + 1;
    
    List.iter (compile_expr ctx buf) then_branch;
    let end_pos = ctx.current_offset in
    emit_opcode buf (List.assoc "JUMP" opcodes);
    emit_u256 buf 0;  (* placeholder *)
    ctx.current_offset <- ctx.current_offset + 1;
    
    (* Patch else jump *)
    let else_jump = Buffer.sub buf 0 else_pos in
    Buffer.truncate buf else_pos;
    Buffer.add_string buf else_jump;
    emit_u256 buf ctx.current_offset;
    
    List.iter (compile_expr ctx buf) else_branch;
    
    (* Patch end jump *)
    let end_jump = Buffer.sub buf 0 end_pos in
    Buffer.truncate buf end_pos;
    Buffer.add_string buf end_jump;
    emit_u256 buf ctx.current_offset

  | Event (name, ty1, ty2, ty3) ->
      Hashtbl.add ctx.symbols name (Event [ty1; ty2; ty3])

  | Emit (name, e1, e2, e3) ->
      let types = match Hashtbl.find_opt ctx.symbols name with
        | Some (Event types) -> types
        | _ -> failwith (Printf.sprintf "Unknown event: %s" name)
      in
      
      compile_expr ctx buf e1;
      compile_expr ctx buf e2;
      compile_expr ctx buf e3;
      
      let signature = Printf.sprintf "%s(%s)" name 
        (String.concat "," (List.map string_of_ty types)) in
      
      let instr_count = compile_string_hash buf signature in
      let name_instr_count = compile_string_hash buf name in
      
      emit_opcode buf (List.assoc "EMIT" opcodes);
      emit_u256 buf 0;  (* topic1 *)
      emit_u256 buf 0;  (* topic2 *)
      emit_u256 buf 0;  (* topic3 *)
      emit_u256 buf 0;  (* data length *)
      
      ctx.current_offset <- ctx.current_offset + instr_count + name_instr_count + 5

  | Abs (arg, _, body) ->
      Hashtbl.add ctx.symbols arg Stack;
      List.iter (compile_expr ctx buf) body;
      emit_opcode buf (List.assoc "RETURN" opcodes);
      ctx.current_offset <- ctx.current_offset + 1;
      Hashtbl.remove ctx.symbols arg

  | App (func, arg) ->
      compile_expr ctx buf arg;
      compile_expr ctx buf func;
      emit_opcode buf (List.assoc "JUMP" opcodes);
      ctx.current_offset <- ctx.current_offset + 1

  | e -> failwith (Printf.sprintf "Not implemented: %s" (string_of_expr e))

let compile_program ctx buf program =
  List.iter (compile_expr ctx buf) program
