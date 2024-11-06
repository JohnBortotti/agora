%{
  open Ast
%}

%token <int> INT
%token <string> STRING
%token <string> IDENT
%token <bool> BOOL
%token PLUS MINUS TIMES DIVIDE
%token LPAREN RPAREN COMMA
%token ARROW LAMBDA ASSIGN VAR
%token EOF IF THEN ELSE SEMICOLON
%token EQ NEQ LT LTE GT GTE COLON STRUCT
%token LET IN UNDERSCORE RBRACKET LBRACKET
%token LBRACE RBRACE MAPPING FN LIST_TYPE DOT
%token BOOL_TYPE INT_TYPE STRING_TYPE TUPLE_TYPE EVENT EMIT

%left PLUS MINUS
%left TIMES DIVIDE

%start main
%type <Ast.program> main
%type <Ast.ty> ty

%%

main:
  | expr_list EOF { $1 }

expr_list:
  | expr SEMICOLON expr_list { $1 :: $3 }
  | expr { [$1] }

tuple_type:
  | ty COMMA tuple_type { $1 :: $3 }
  | ty COMMA ty { [$1; $3] }

tuple_elements:
  | expr COMMA tuple_elements { $1 :: $3 }
  | expr { [$1] }

list_elements:
  | expr SEMICOLON list_elements { $1 :: $3 }
  | expr { [$1] }

expr:
  | e1 = expr e2 = term { App(e1, e2) }
  | IF e1 = expr THEN e2 = expr_list ELSE e3 = expr_list { If(e1, e2, e3) }
  | LET IDENT EQ LBRACE e1 = expr_list RBRACE { Let_brace($2, e1) }
  | LET IDENT EQ e1 = expr IN e2 = expr { Let($2, e1, e2) }
  | VAR IDENT COLON t = ty COLON EQ e1 = expr { VarBind($2, t, e1) }
  | MAPPING LPAREN t1 = ty COMMA t2 = ty RPAREN IDENT  { Mapping($7, t1, t2) }
  | EVENT IDENT LPAREN t1 = ty COMMA t2 = ty COMMA t3 = ty RPAREN { Event($2, t1, t2, t3) }
  | LBRACKET expr_list RBRACKET { List($2) }
  | LPAREN tuple_elements RPAREN { Tuple($2) }
  | LBRACKET list_elements RBRACKET { List($2) }
  | e1 = expr COLON EQ e2 = expr { Assign (e1, e2) }
  | e1 = expr PLUS e2 = expr { BinOp(Add, e1, e2) }
  | e1 = expr MINUS e2 = expr { BinOp(Sub, e1, e2) }
  | e1 = expr TIMES e2 = expr { BinOp(Mul, e1, e2) }
  | e1 = expr DIVIDE e2 = expr { BinOp(Div, e1, e2) }
  | e1 = expr EQ e2 = expr { BinOp(Eq, e1, e2) }
  | e1 = expr NEQ e2 = expr { BinOp(Neq, e1, e2)}
  | e1 = expr LT e2 = expr { BinOp(Lt, e1, e2) }
  | e1 = expr LTE e2 = expr { BinOp(Lte, e1, e2) }
  | e1 = expr GT e2 = expr { BinOp(Gt, e1, e2) }
  | e1 = expr GTE e2 = expr { BinOp(Gte, e1, e2) }
  | t = term { t }

term:
  | IDENT { Var $1 }
  | INT { Int $1 }
  | BOOL { Bool $1 }
  | STRING { String $1 }
  | LAMBDA IDENT COLON t = ty ARROW x = expr_list { Abs($2, t, x) }
  | LPAREN e = expr RPAREN { e }
  | IDENT LBRACKET expr RBRACKET { IndexAccess(Var $1, $3) }
  | IDENT DOT expr { TupleAccess(Var $1, $3) }
  | EMIT IDENT LPAREN e1 = expr COMMA e2 = expr COMMA e3 = expr RPAREN { Emit($2, e1, e2, e3) }

ty:
  | LPAREN t1 = ty ARROW t2 = ty RPAREN { TArrow(t1, t2) }
  | LPAREN t = tuple_type RPAREN TUPLE_TYPE { TTuple(t) }
  | STRING_TYPE { TString }
  | BOOL_TYPE { TBool }
  | INT_TYPE { TInt }
  | t = ty LIST_TYPE { TList t }
