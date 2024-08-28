(*
node implementation:
  - [ ] state 
    - [ ] account global state (funds, nonce, storageRoot, codeHash)
    - [ ] contract storage
  - [ ] main loop
    - [x] handle incoming transactions
    - [x] list transaction_pool
    - [ ] mine
      - [x] filter validated txs
      - [x] write and hash block
      - [x] compute nonce
      - [x] calculate difficulty based on prev blocks
      - [x] append mined_block
      - [ ] "Lwt_mvar.take node.blockchain" -> use a mutex instead
      - [ ] broadcast mined block
    - [ ] handle incoming block proposal 
      - [x] suspend current block mining (node.mining)
      - [x] validate block
      - [ ] broadcast proposed block (if valid)
    - [ ] validate transaction
    - [ ] gossip protocol
 *)
open Transaction
open State
open Lwt
open Lwt.Syntax
open Cohttp_lwt_unix

type node = {
  address: string;
  transaction_pool: (Transaction.transaction * bool) list Lwt_mvar.t;
  blockchain: Block.block list Lwt_mvar.t;
  mining: bool Lwt_mvar.t;
  miner_addr: string;
  global_state: MKPTrie.trie;
  known_peers: string list Lwt_mvar.t;
}

let add_transaction pool tx =
  let* current_pool = Lwt_mvar.take pool in
  let updated_pool = (tx, false) :: current_pool in
  Lwt_mvar.put pool updated_pool
  
let rec validate_transaction_pool node =
  print_endline "pool validation\n";
  let* current_pool = Lwt_mvar.take node.transaction_pool in
  let new_pool = List.filter_map (fun (tx, verified) -> 
    if not verified then 
      if Transaction.validate_transaction tx then (
        print_endline "verifying tx";
        Some (tx, true)
      )
      else (
        print_endline "tx invalid";
        None
      )
    else (
      print_endline "tx already verified";
      Some (tx, verified)
    )
  ) current_pool in
  let* _ = Lwt_mvar.put node.transaction_pool new_pool in
  let* _ = Lwt_unix.sleep 4.0 in
  validate_transaction_pool node

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

let block_of_json json: Block.block =
  let open Yojson.Basic.Util in
  {
    index = json |> member "index" |> to_int;
    previous_hash = json |> member "previous_hash" |> to_string;
    timestamp = json |> member "timestamp" |> to_float;
    transactions = json |> member "transactions" |> to_list |> List.map transaction_of_json;
    miner = json |> member "miner" |> to_string;
    nonce = json |> member "nonce" |> to_int;
    hash = json |> member "hash" |> to_string;
    difficulty = json |> member "difficulty" |> to_int;
  }

let handle_transaction_request (node: node) (body: string) =
  let json = Yojson.Basic.from_string body in
  let tx = transaction_of_json json in
  add_transaction node.transaction_pool tx

let handle_block_proposal_request node body =
  let received_block = Yojson.Basic.from_string body |> block_of_json in
  Lwt_mvar.take node.blockchain >>= fun curr_chain ->
  let prev_block = List.hd curr_chain in
  if Block.validate_block received_block prev_block then
    let new_chain = received_block :: curr_chain in
    let* _ = Lwt_mvar.put node.blockchain new_chain in
    Lwt.return_true
  else 
    let* _ = Lwt_mvar.put node.blockchain curr_chain in
    Lwt.return_false 

let mine_block transactions prev_block difficulty miner_addr =
  let open Block in
  let rec mine nonce =
    let candidate_block = {
      index = prev_block.index + 1;
      previous_hash = prev_block.hash;
      timestamp = Unix.time ();
      transactions = transactions;
      miner = miner_addr;
      nonce = nonce;
      difficulty = difficulty;
      hash = ""
    } in
    let candidate_hash = hash_block { candidate_block with hash = "" } in 
    if is_valid_pow candidate_hash difficulty then
      { candidate_block with hash = candidate_hash }
    else
      mine (nonce + 1)
    in mine 0

let mining_routine node = 
  let threshold = 2 in
  let time_delay = 10.0 in
  let rec aux () = 
    let* mining = Lwt_mvar.take node.mining in
    if mining then
      (print_endline "mining enabled\n";
      let* _ = Lwt_mvar.put node.mining mining in 
      loop ())
    else
      (print_endline "mining disabled\n";
      let* _ = Lwt_mvar.put node.mining mining in 
      let* _ = Lwt_unix.sleep time_delay in
      aux ())
  and
  loop () =
    Lwt_unix.sleep time_delay >>= fun () ->
      Lwt_mvar.take node.transaction_pool >>= fun curr_pool ->
        let validated_transactions, remaining_transactions = 
          List.partition (fun (_, verified) -> verified) curr_pool in
        if List.length validated_transactions >= threshold then (
          print_endline "\nstarted mining block\n";
          let transactions_to_mine = List.map fst validated_transactions in
          Lwt_mvar.take node.blockchain >>= fun curr_blockchain ->
            let prev_block = List.hd curr_blockchain in
            let difficulty = Block.calculate_difficulty prev_block in
            let miner_addr = node.miner_addr in
            let mined_block = mine_block transactions_to_mine prev_block difficulty miner_addr in
            print_endline "\nBlock mined:\n"; 
            print_endline (Block.string_of_block mined_block);
            let* _ = Lwt_mvar.put node.transaction_pool remaining_transactions in
            let new_chain = mined_block :: curr_blockchain in
            let* _ = Lwt_mvar.put node.blockchain new_chain in
          aux ()
        ) else (
          print_endline "mine pass...\n";
          let* _ = Lwt_mvar.put node.transaction_pool curr_pool in
          aux ()
          )
    in
    aux ()

let http_server node =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.path in
    match (Request.meth req, uri) with
    | (`POST, "/transaction") ->
        print_endline "\ntransaction received\n";
        body |> Cohttp_lwt.Body.to_string >>= fun body ->
        handle_transaction_request node body >>= fun () ->
        Server.respond_string ~status:`OK ~body:"Transaction received" ()
    | (`POST, "/block") ->
        print_endline "\nblock proposal received\n";
        let* _ = Lwt_mvar.take node.mining in
        let* _ = Lwt_mvar.put node.mining false in
        print_endline "mining = false\n";
        body |> Cohttp_lwt.Body.to_string >>= fun body ->
        let* result = handle_block_proposal_request node body in
        let* _ = Lwt_mvar.take node.mining in
        let* _ = Lwt_mvar.put node.mining true in
        print_endline "mining = true\n";
        (match result with
        | true -> Server.respond_string ~status:`OK ~body:"Block received is valid" ()
        | false -> Server.respond_string ~status:`OK ~body:"Block received invalid" ())
    | (`GET, "/gossip") ->
        let* peers_list = Lwt_mvar.take node.known_peers in
        let json_peers = `List (List.map 
          (fun peer -> `String (String.trim peer)) peers_list) 
        in
        let json_body = Yojson.Basic.to_string json_peers in
        let* _ = Lwt_mvar.put node.known_peers peers_list in
        Server.respond_string ~status:`OK ~body:json_body ()
    | (`GET, "/transaction_pool") ->
        Lwt_mvar.take node.transaction_pool >>= fun pool ->
        let tx_json_list = List.map (fun (tx, _) -> 
        Transaction.transaction_to_json tx) pool in
        let json_body = `List tx_json_list |> Yojson.Basic.to_string in
        Lwt_mvar.put node.transaction_pool pool >>= fun () ->
        Server.respond_string ~status:`OK ~body:json_body ()
    | (`GET, "/chain") ->
        Lwt_mvar.take node.blockchain >>= fun chain ->
        let chain_json_list = List.map (fun bl -> Block.block_to_json bl) chain in
        let json_body = `List chain_json_list |> Yojson.Basic.to_string in
        Lwt_mvar.put node.blockchain chain >>= fun () ->
        Server.respond_string ~status:`OK ~body:json_body ()
    | _ ->
        Server.respond_string ~status:`Not_found ~body:"Not found" ()
  in
  let server = Server.make ~callback () in
  Server.create ~mode:(`TCP (`Port 8080)) server

let run_node node =
  let main_loop =
    Lwt.join [
      validate_transaction_pool node;
      mining_routine node;
      http_server node
    ] in
  Lwt_main.run main_loop
