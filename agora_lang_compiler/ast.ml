type expr =
  | Int of int
  | Bool of bool
  | String of string
  | Var of string
  | VarBind of string * expr
  | Abs of string * expr list
  | App of expr * expr
  | If of expr * expr list * expr list
  | Let of string * expr * expr
  | BinOp of binop * expr * expr
  | IndexAccess of expr * expr
  | Assign of expr * expr
  | List of expr list

and binop = Add | Sub | Mul | Div | Eq | Neq | Lt | Lte | Gt | Gte

type program = expr list

let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Eq -> "="
  | Neq -> "!="
  | Lt -> "<"
  | Lte -> "<="
  | Gt -> ">"
  | Gte -> ">="

let rec string_of_expr = function
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | String s -> Printf.sprintf "\"%s\"" s
  | Var x -> Printf.sprintf "Var (%s)" x
  | VarBind (x, e) -> 
      Printf.sprintf "VarBind (%s := %s)" x (string_of_expr e)
  | Abs (x, body) -> 
      Printf.sprintf "Abs (%s, [%s])" x (String.concat "; " (List.map string_of_expr body))
  | App (e1, e2) -> 
      Printf.sprintf "App (%s %s)" (string_of_expr e1) (string_of_expr e2)
  | Let (x, e1, e2) -> 
      Printf.sprintf "Let %s = %s in %s" x (string_of_expr e1) (string_of_expr e2)
  | If (cond, then_branch, else_branch) -> 
      Printf.sprintf "If (%s) then [%s] else [%s]" 
        (string_of_expr cond) 
        (String.concat "; " (List.map string_of_expr then_branch)) 
        (String.concat "; " (List.map string_of_expr else_branch))
  | BinOp (op, e1, e2) -> 
      Printf.sprintf "BinOp (%s %s %s)" (string_of_expr e1) (string_of_binop op) (string_of_expr e2)
  | IndexAccess (e1, e2) -> 
      Printf.sprintf "IndexAccess (%s[%s])" (string_of_expr e1) (string_of_expr e2)
  | Assign (e1, e2) -> 
      Printf.sprintf "Assign (%s := %s)" (string_of_expr e1) (string_of_expr e2)
  | List l -> 
      Printf.sprintf "List ([%s])" (String.concat "; " (List.map string_of_expr l))
