(*
TODO:
  - [ ] type-check
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
        let ast = Parser.main Lexer.read lexbuf in
        close_in channel;
        (* List.iter (fun expr -> Printf.printf "%s\n\n" (Ast.string_of_expr expr)) ast; *)

        let type_env = Hashtbl.create 10 in
        let type_check_exprs exprs =
          List.iter (fun expr ->
            try
              let inferred_type = Ast.check_type_expr type_env expr in
              Printf.printf "Expression: %s\nType: %s\n\n"
                (Ast.string_of_expr expr)
                (Ast.string_of_ty inferred_type)
            with Failure msg ->
              Printf.eprintf "Type error: %s\nExpression: %s\n\n%!" msg (Ast.string_of_expr expr);
              exit 1
          ) exprs
        in
        type_check_exprs ast
         
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
