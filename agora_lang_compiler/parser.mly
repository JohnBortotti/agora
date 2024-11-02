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
%token EQ NEQ LT LTE GT GTE COLON
%token LET IN UNDERSCORE RBRACKET LBRACKET

%left PLUS MINUS
%left TIMES DIVIDE

%start main
%type <Ast.program> main

%%

main:
  | expr_list EOF { $1 }

expr_list:
  | expr SEMICOLON expr_list { $1 :: $3 }
  | expr { [$1] }

expr:
  | e1 = expr e2 = term { App(e1, e2) }
  | IF e1 = expr THEN e2 = expr_list ELSE e3 = expr_list { If(e1, e2, e3) }
  | VAR IDENT ASSIGN e1 = expr SEMICOLON { VarBind($2, e1) }
  | LET IDENT EQ e1 = expr IN e2 = expr { Let($2, e1, e2) }
  | VAR IDENT COLON EQ e1 = expr { VarBind($2, e1) }
  | LBRACKET expr_list RBRACKET { List($2) }
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
  | LAMBDA IDENT ARROW expr_list { Abs ($2, $4) }
  | LPAREN e = expr RPAREN { e }
  | IDENT LBRACKET expr RBRACKET { IndexAccess(Var $1, $3) }
