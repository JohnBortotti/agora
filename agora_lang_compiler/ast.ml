type ty = 
  | TUnit
  | TBool
  | TInt
  | TString
  | TArrow of ty * ty
  | TMapping of ty * ty
  | TTuple of ty list

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
  | Let_brace of string * expr list
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
  | TUnit -> "unit"
  | TBool -> "bool"
  | TInt -> "int"
  | TString -> "string"
  | TArrow (t1, t2) -> Printf.sprintf "abs (%s -> %s)" (string_of_ty t1) (string_of_ty t2)
  | TMapping (t1, t2) -> Printf.sprintf "mapping(%s, %s)" (string_of_ty t1) (string_of_ty t2)
  | TTuple l -> Printf.sprintf "tuple (%s)" (String.concat ", " (List.map string_of_ty l))

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
  | Let_brace (x, e) -> 
      Printf.sprintf "Let %s = [%s]" x (String.concat "; " (List.map string_of_expr e))
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

type type_env = (string, ty) Hashtbl.t

let type_of_var env name =
  try Hashtbl.find env name
  with Not_found -> failwith ("Unbound variable: " ^ name)

let check_type expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "Type error: expected %s but got %s"
    (string_of_ty expected) (string_of_ty actual))

let rec check_type_expr (env: type_env) (expr: expr) : ty = 
  match expr with
  | Int _ -> TInt
  | Bool _ -> TBool
  | String _ -> TString
  | Tuple l -> TTuple (List.map (check_type_expr env) l)
  | Var x -> type_of_var env x
  | Mapping (x, t1, t2) -> 
      Hashtbl.add env x (TMapping (t1, t2));
      TMapping (t1, t2)
  | VarBind (x, ty, e) -> 
      Hashtbl.add env x ty;
      check_type_expr env e
  | Abs (x, ty, body) -> 
      Hashtbl.add env x ty;
      let t = List.map (fun expr -> check_type_expr env expr) body in
      let tl = List.hd (List.rev t) in
      TArrow (ty, tl)
  | App (e1, e2) -> 
      let func_type = check_type_expr env e1 in
      let arg_type = check_type_expr env e2 in
      (match func_type with
       | TArrow (param_type, return_type) -> 
           check_type param_type arg_type; 
           return_type
       | _ -> failwith "Type error: expected function type")
  | If (cond, then_branch, else_branch) -> 
      check_type TBool (check_type_expr env cond);
      let t_then = List.map (fun expr -> check_type_expr env expr) then_branch in
      let t_else = List.map (fun expr -> check_type_expr env expr) else_branch in
      let then_type = List.hd (List.rev t_then) in
      let else_type = List.hd (List.rev t_else) in
      check_type then_type else_type;
      then_type
  | Let (x, e1, e2) -> 
      let t1 = check_type_expr env e1 in
      Hashtbl.add env x t1;
      check_type_expr env e2
  | Let_brace (x, e) -> 
      let t = List.map (fun expr -> check_type_expr env expr) e in
      let tl = List.hd (List.rev t) in
      Hashtbl.add env x tl;
      tl
  | BinOp (op, e1, e2) -> 
      let t1 = check_type_expr env e1 in
      let t2 = check_type_expr env e2 in
      (match op with
       | Add | Sub | Mul | Div -> 
           check_type TInt t1;
           check_type TInt t2;
           TInt
       | Eq | Neq -> 
           check_type t1 t2;
           TBool
       | Lt | Lte | Gt | Gte -> 
           check_type TInt t1;
           check_type TInt t2;
           TBool)
  | IndexAccess (e1, e2) -> 
      let mapping_type = check_type_expr env e1 in
      let key_type = check_type_expr env e2 in
      (match mapping_type with
       | TMapping (expected_key_type, value_type) -> 
           check_type expected_key_type key_type; 
           value_type
       | _ -> failwith "Type error: expected mapping type")
  | Assign (e1, e2) -> 
      let var_type = check_type_expr env e1 in
      let value_type = check_type_expr env e2 in
      check_type var_type value_type;
      TUnit
  | List l -> 
      (* TODO: should every expr be of same type? *)
      TTuple (List.map (check_type_expr env) l)

