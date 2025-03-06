{
  open Parser
  exception Error of string

  let string_buff = Buffer.create 256
}

rule read = parse
  | [' ' '\t' '\r' '\n'] { read lexbuf }
  | ['0'-'9']+ as lxm { INT (int_of_string lxm) }
  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { 
    match lxm with 
    | "bool"    -> BOOL_TYPE
    | "int"     -> INT_TYPE
    | "string"  -> STRING_TYPE
    | "list"    -> LIST_TYPE
    | "tuple"   -> TUPLE_TYPE
    | "false"   -> BOOL false
    | "true"    -> BOOL true
    | "let"     -> LET
    | "in"      -> IN
    | "if"      -> IF
    | "then"    -> THEN
    | "else"    -> ELSE
    | "var"     -> VAR
    | "mapping" -> MAPPING
    | "event"   -> EVENT
    | "emit"    -> EMIT
    | "error"   -> ERROR
    | "publish" -> PUBLISH
    | "view"    -> VIEW
    | _         -> IDENT lxm
  }
  | [' ' '\t' '\r' '\n']       { read lexbuf }
  | "(*"                       { comment lexbuf }
  | '\"'
      { Buffer.clear string_buff;
        string lexbuf;
        STRING (Buffer.contents string_buff) }
  | '\\'                       { LAMBDA }
  | ','                        { COMMA }
  | '+'                        { PLUS }
  | '-'                        { MINUS }
  | '_'                        { UNDERSCORE }
  | '*'                        { TIMES }
  | '/'                        { DIVIDE }
  | '='                        { EQ }
  | "!="                       { NEQ }
  | '<'                        { LT }
  | "<="                       { LTE }
  | '>'                        { GT }
  | ">="                       { GTE }
  | '('                        { LPAREN }
  | ')'                        { RPAREN }
  | '['                        { LBRACKET }
  | ']'                        { RBRACKET }
  | ';'                        { SEMICOLON }
  | '{'                        { LBRACE }
  | '}'                        { RBRACE }
  | ':'                        { COLON }
  | '.'                        { DOT }
  | "->"                       { ARROW }
  | ['0'-'9']+ as i            { INT (int_of_string i) }
  | eof                        { EOF }
  | _ as c                     { raise (Error (Printf.sprintf "Unknown character: %c" c)) }

and comment = parse
  | "*)"                       { read lexbuf }
  | eof                        { raise (Error "Unterminated comment") }
  | _                          { comment lexbuf }

and string = parse
  | '\"'  { () }
  | _ as c 
      { Buffer.add_char string_buff c;
        string lexbuf }
