open Ast
open Digestif.SHA256

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
  Buffer.add_string buf (Printf.sprintf "%02x" opcode)

let emit_u256 buf value =
  let hex_value = Printf.sprintf "%064x" value in
  Buffer.add_string buf hex_value

let string_to_u256_chunks s =
  (* convert string to hex representation *)
  let hex_str = String.concat "" (
    List.map (fun c -> Printf.sprintf "%02x" (Char.code c))
    (List.init (String.length s) (String.get s))
  ) in
  
  (* split into 64-char chunks, padding each *)
  let rec to_chunks acc s =
    if s = "" then List.rev acc
    else
      let chunk_size = min 64 (String.length s) in
      let chunk = String.sub s 0 chunk_size in
      (* pad this chunk to 64 chars *)
      let padded_chunk = chunk ^ String.make (64 - chunk_size) '0' in
      let rest = if chunk_size >= String.length s then ""
                 else String.sub s chunk_size (String.length s - chunk_size) in
      to_chunks (padded_chunk :: acc) rest
  in
  to_chunks [] hex_str
  
let compile_string_hash buf s =
  let chunks = string_to_u256_chunks s in
  List.iter (fun chunk ->
    emit_opcode buf (List.assoc "PUSH" opcodes);
    Buffer.add_string buf chunk
  ) chunks;
  emit_opcode buf (List.assoc "SHA256" opcodes);
  emit_u256 buf (List.length chunks);
  List.length chunks + 1

type storage_var = 
  | StateVar
  | Mapping
  | Stack
  | Function of (int * expr list)
  | Event of ty list

type dispatcher_entry = {
  hash: string;
  offset: int;
}

type compilation_context = {
  symbols: (string, storage_var) Hashtbl.t;
  mutable current_offset: int;
  publish_functions: dispatcher_entry list;
  view_functions: dispatcher_entry list;
}

let create_context () = {
  symbols = Hashtbl.create 64;
  current_offset = 0;
  publish_functions = [];
  view_functions = [];
}

let rec get_function_params expr =
  match expr with
  | Abs(arg, ty, body) -> 
      let remaining_params = get_function_params (List.hd body) in
      (arg, ty) :: remaining_params
  | _ -> []

let rec get_param_type = function
  | TUnit -> "unit"
  | TBool -> "bool"
  | TInt -> "int"
  | TString -> "string"
  | TArrow(param_type, return_type) -> get_param_type param_type
  | _ -> failwith "Unsupported type in function signature"

let make_function_signature name expr =
  let rec get_params = function
    | Abs(arg, ty, body) -> 
        let remaining_params = match body with
          | [e] -> get_params e
          | _ -> []
        in
        (get_param_type ty) :: remaining_params
    | _ -> []
  in
  let param_types = get_params expr in
  let param_str = String.concat "," param_types in
  let hash = Digestif.SHA256.digest_string param_str in
  let hex = Digestif.SHA256.to_hex hash in
  hex

(* linear dispatcher O(n) *)
let generate_dispatcher ctx buf: int =
  let instruction_count = ref 0 in

  (* TODO: emit opcode to get hashed input *)
  emit_opcode buf (List.assoc "PUSH" opcodes);
  emit_u256 buf 0;
  incr instruction_count;

  List.iter (fun func ->
    emit_opcode buf (List.assoc "PUSH" opcodes);
    Buffer.add_string buf func.hash;
    incr instruction_count;

    (* compare hashes *)
    emit_opcode buf (List.assoc "EQ" opcodes);
    incr instruction_count;

    (* if match, jump *)
    emit_opcode buf (List.assoc "PUSH" opcodes);
    emit_u256 buf func.offset;
    incr instruction_count;

    emit_opcode buf (List.assoc "JUMPI" opcodes);
    incr instruction_count;
  ) ctx.publish_functions;

  emit_opcode buf (List.assoc "HALT" opcodes);
  incr instruction_count;

  !instruction_count

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
      | Some (Function (offset, _)) ->
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
      Hashtbl.add ctx.symbols name (Function (func_offset, body));
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
      let _ = match Hashtbl.find_opt ctx.symbols name with
        | Some (Event types) -> types
        | _ -> failwith (Printf.sprintf "Unknown event: %s" name)
      in
      
      compile_expr ctx buf e1;
      compile_expr ctx buf e2;
      compile_expr ctx buf e3;
      let instr_count = compile_string_hash buf name in

      emit_opcode buf (List.assoc "EMIT" opcodes);

      ctx.current_offset <- ctx.current_offset + 1 + instr_count

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

  (* | View expr_l ->  *)
  (*     let functions = List.map  *)
  (*       (fun e -> match e with *)
  (*       | Var n ->    *)
  (*       | _ -> Printf.sprinft "Not implemented: non-var on view[]" *)
  (*     ) in *)
  (**)
  (*     List.append ctx.dispatcher_functions *)
  (*     print_endline "registering view functions"; *)
  (*     List.iter (fun f -> print_endline (string_of_expr f)) expr_l; *)
  (*     print_endline "" *)

  | Publish expr_l -> 
      (* process each published function with adjusted offsets *)
      let functions = List.map (fun e -> match e with
        | Var name -> 
            let offset, f_expr = match Hashtbl.find_opt ctx.symbols name with
              | Some (Function (orig_offset, f_expr)) -> 
                  (orig_offset, f_expr)
              | _ -> failwith (Printf.sprintf "Unknown function: %s" name)
            in
            let hash = match f_expr with
              | expr :: _ -> make_function_signature name expr
              | [] -> failwith "Empty function body"
            in
            { hash; offset }
        | _ -> failwith "Only variables can be published"
      ) expr_l in
      
      (* get dispatcher size to calculate final offsets *)
      let temp_ctx = { ctx with publish_functions = functions } in
      let dispatcher_buf = Buffer.create 256 in
      let dispatcher_size = generate_dispatcher temp_ctx dispatcher_buf in
      Buffer.clear dispatcher_buf;

      (* update context with functions with adjusted offsets *)
      let ctx = { ctx with
        publish_functions = List.map (fun f -> 
          { f with offset = f.offset + dispatcher_size }
        ) functions
      } in

      (* save current program to temporary buffer *)
      let program_buf = Buffer.create (Buffer.length buf) in
      Buffer.add_buffer program_buf buf;

      (* clear current buffer *)
      Buffer.clear buf;

      (* generate final dispatcher with correct offsets *)
      let _ = generate_dispatcher ctx dispatcher_buf in

      (* fill current buffer with dispatcher followed by program *)
      Buffer.add_buffer buf dispatcher_buf;
      Buffer.add_buffer buf program_buf;
      
      ctx.current_offset <- ctx.current_offset + dispatcher_size;

  | e -> failwith (Printf.sprintf "Not implemented: %s" (string_of_expr e))

let compile_program ctx buf program =
  List.iter (compile_expr ctx buf) program
