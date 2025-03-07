let () =
  if Array.length Sys.argv < 2 then
    Printf.eprintf "Usage: %s <filename> [--debug]\n%!" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    let debug = Array.length Sys.argv > 2 && Sys.argv.(2) = "--debug" in
    try
      let channel = open_in filename in
      let lexbuf = Lexing.from_channel channel in
      try
        (* expression parsing *)
        let ast = Parser.main Lexer.read lexbuf in
        close_in channel;
        (* List.iter (fun expr -> Printf.printf "%s\n\n" (Ast.string_of_expr expr)) ast; *)

        (* add reserved functions on env *)
        let type_env: Ast.type_env = Hashtbl.create 10 in
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

        (* check duplicated calls for "publish" *)
        List.fold_left (fun acc expr ->
          match expr with
          | Ast.Publish _ -> acc + 1
          | _ -> acc
        ) 0 ast |> (fun n -> if n > 1 then raise (Failure "Duplicated calls for 'publish'"));
        (* check duplicated calls for "view" *)

        (* TODO: check if args of "publish" are Abstractions *)

        (* check duplicated calls for "view" *)
        List.fold_left (fun acc expr ->
          match expr with
          | Ast.View _ -> acc + 1
          | _ -> acc
        ) 0 ast |> (fun n -> if n > 1 then raise (Failure "Duplicated calls for 'view'"));

        (* TODO: check if args of "view" are Abstractions *)
        (* TODO: check if functions of "view" are pure *)

        let opcode_buf = Buffer.create 256 in
        let debug_buf = if debug then Buffer.create 1024 else Buffer.create 0 in
        let ctx = Code_gen.create_context () in
        Code_gen.compile_program ctx opcode_buf debug_buf ast debug;
        if debug then
          print_endline (Buffer.contents debug_buf);
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
