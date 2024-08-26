(*
node implementation:
  - [ ] state 
    - [ ] account global state (funds, nonce, storageRoot, codeHash)
    - [ ] contract storage
  - [ ] main loop
    - [x] handle incoming transactions
      - [ ] validate tx
      - [x] append on transaction_pool
    - [x] list transaction_pool (debug)
    - [ ] mine
      - [ ] filter validated txs
      - [ ] write and hash block
      - [ ] compute nonce
      - [ ] broadcast block
    - [ ] handle incoming block proposal 
      - [ ] suspend currenct block
      - [ ] validate block
      - [ ] broadcast new block
 *)
open Transaction
open State
open Lwt
open Lwt.Syntax
open Cohttp_lwt_unix

type node = {
  transaction_pool: (Transaction.transaction * bool) list Lwt_mvar.t;
  global_state: MKPTrie.trie
}

let add_transaction pool tx =
  Lwt_mvar.take pool >>= fun current_pool ->
  let updated_pool = (tx, false) :: current_pool in
  Lwt_mvar.put pool updated_pool
  
let validate_transaction _tx = true

(* let validate_transaction_pool pool = *)
(*   Lwt_mvar.take pool >>= fun current_pool -> *)
(*     let new_pool = List.map (fun (tx, verified) ->  *)
(*       if not verified then (tx, validate_transaction tx) else (tx, verified) *)
(*     ) current_pool in *)
(*     Lwt_mvar.put pool new_pool *)

let transaction_of_json json: Transaction.transaction =
  let open Yojson.Basic.Util in
  {
    sender = json |> member "sender" |> to_string;
    receiver = json |> member "receiver" |> to_string;
    amount = json |> member "amount" |> to_int;
    gas_limit = 1;
    gas_price = 1;
    nonce = 1;
    payload = "";
    signature = ""
  }

let handle_transaction_request (node: node) (body: string) =
  let json = Yojson.Basic.from_string body in
  let tx = transaction_of_json json in
  if validate_transaction tx then
    add_transaction node.transaction_pool tx
  else
    Lwt.return_unit

let get_valid_transactions pool =
  Lwt_mvar.take pool >>= fun current_pool ->
  let valid_transactions = List.filter (fun (_, is_valid) -> is_valid) current_pool in
  let* () = Lwt_mvar.put pool current_pool in
  Lwt.return (List.map fst valid_transactions)

let server node =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.path in
    match (Request.meth req, uri) with
    | (`POST, "/transaction") ->
        body |> Cohttp_lwt.Body.to_string >>= fun body ->
        handle_transaction_request node body >>= fun () ->
        Server.respond_string ~status:`OK ~body:"Transaction received" ()
    | (`GET, "/transactions") ->
        Lwt_mvar.take node.transaction_pool >>= fun pool ->
          let tx_str = List.map (fun (tx, _) ->
            Transaction.transaction_to_json_string tx
          ) pool in
          let response_body = Printf.sprintf "transaction_pool: [\n%s\n]\n" (String.concat ",\n" tx_str) in
          Lwt_mvar.put node.transaction_pool pool >>= fun () ->
          Server.respond_string ~status:`OK ~body:response_body ()
    | _ ->
        Server.respond_string ~status:`Not_found ~body:"Not found" ()
  in
  let server = Server.make ~callback () in
  Server.create ~mode:(`TCP (`Port 8080)) server

let run_node node =
  let main_loop =
    Lwt.join [
      server node
    ] in
  Lwt_main.run main_loop
