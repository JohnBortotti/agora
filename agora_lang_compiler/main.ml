(*
TODO:
  - [x] event type
  - [ ] VM change addrs strings to U256 (convert hex)
  - [ ] hash function (hash strings, event signature)
  - [ ] opcode emit will hash the event signature
  - [ ] list, tuple and mapping functions (folds)
  - [ ] code-gen
*)
let () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: %s <filename>\n%!" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    try
      let channel = open_in filename in
      let lexbuf = Lexing.from_channel channel in
      try
        (* expression parsing *)
        let ast = Parser.main Lexer.read lexbuf in
        Printf.printf "Parsing passed\n\n";
        close_in channel;
        (* List.iter (fun expr -> Printf.printf "%s\n\n" (Ast.string_of_expr expr)) ast; *)

        (* add reserved functions on env *)
        let type_env: Ast.type_env = Hashtbl.create 10 in
        Hashtbl.add type_env "publish" (Ast.TArrow (Ast.TList Ast.TString, Ast.TUnit));
        Hashtbl.add type_env "view" (Ast.TArrow (Ast.TList Ast.TString, Ast.TUnit));
        (* Hashtbl.add type_env "emit" (Ast.TArrow (Ast.TString, Ast.TUnit)); *)
        Hashtbl.add type_env "error" (Ast.TArrow (Ast.TString, Ast.TUnit));
        Hashtbl.add type_env "get_address_balance" (Ast.TArrow (Ast.TString, Ast.TInt));

        (* type-checking *)
        let type_check_exprs exprs =
          List.iter (fun expr ->
            try
              (* let inferred_type = Ast.check_type_expr type_env expr in *)
              (* Printf.printf "Expression: %s\nType: %s\n\n" *)
              (*   (Ast.string_of_expr expr) *)
              (*   (Ast.string_of_ty inferred_type) *)
              let _ = Ast.check_type_expr type_env expr in ()
            with Failure msg ->
              Printf.eprintf "Type error: %s\nExpression: %s\n\n%!" msg (Ast.string_of_expr expr);
              exit 1
          ) exprs
        in
        type_check_exprs ast;
        Printf.printf "Type check passed\n\n";

        (* check duplicated calls for "publish" *)
        Ast.check_duplicated_call "publish" ast;

        (* check if args of "publish" are Abstractions *)
        Ast.check_arg_list_is_type_abs "publish" type_env ast;

        (* check duplicated calls for "view" *)
        Ast.check_duplicated_call "view" ast;

        (* check if args of "view" are Abstractions *)
        Ast.check_arg_list_is_type_abs "view" type_env ast;

        (* check if functions of "view" are pure *)
        Ast.check_pure_functions "view" type_env ast;

        let opcode_buf = Buffer.create 100 in
        let ctx = Code_gen.create_context () in
        Code_gen.compile_program ctx opcode_buf ast;
        print_endline (Buffer.contents opcode_buf)

      with
      | Lexer.Error msg ->
          close_in channel;
          Printf.eprintf "Lexer error: %s\n%!" msg
      | Parser.Error ->
          close_in channel;
          let pos = lexbuf.Lexing.lex_curr_p in
          Printf.eprintf "Syntax error at line %d, column %d\n%!"
            pos.Lexing.pos_lnum
            (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
    with
    | Sys_error msg ->
        Printf.eprintf "Cannot open file '%s': %s\n%!" filename msg
