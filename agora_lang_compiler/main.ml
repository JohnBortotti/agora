let () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: %s <filename>\n%!" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    try
      let channel = open_in filename in
      let lexbuf = Lexing.from_channel channel in
      try
        let result = Parser.main Lexer.read lexbuf in
        close_in channel;
        List.iter (fun expr -> Printf.printf "%s\n\n" (Ast.string_of_expr expr)) result
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
