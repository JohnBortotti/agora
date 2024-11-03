type ty = 
  | TBool
  | TInt
  | TString
  | TArrow of ty * ty
  | TVar of string

type expr =
  | Int of int
  | Bool of bool
  | String of string
  | Tuple of expr list
  | Var of string
  | Mapping of string * ty * ty
  | VarBind of string * ty * expr
  | Abs of string * ty * expr list
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

let rec string_of_ty = function
  | TBool -> "bool"
  | TInt -> "int"
  | TString -> "string"
  | TArrow (t1, t2) -> Printf.sprintf "(%s -> %s)" (string_of_ty t1) (string_of_ty t2)
  | TVar x -> x

let rec string_of_expr = function
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | String s -> Printf.sprintf "\"%s\"" s
  | Tuple l ->  Printf.sprintf "Tuple (%s)" (String.concat "; " (List.map string_of_expr l))
  | Var x -> Printf.sprintf "Var (%s)" x
  | Mapping (x, t1, t2) -> 
      Printf.sprintf "Mapping (%s: (%s, %s))" x
      (string_of_ty t1)
      (string_of_ty t2)
  | VarBind (x, ty, e) -> 
      Printf.sprintf "VarBind (%s: %s := %s)" x  (string_of_ty ty) (string_of_expr e)
  | Abs (x, ty, body) ->
      Printf.sprintf "Abs (%s: %s) -> [%s]" x 
      (string_of_ty ty) (String.concat "; " (List.map string_of_expr body))
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
