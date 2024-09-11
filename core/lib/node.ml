(*
node implementation:
  - [x] main loop
    - [x] handle incoming transactions
    - [x] list transaction_pool
    - [x] mine
      - [x] filter validated txs
      - [x] write and hash block
      - [x] compute nonce
      - [x] calculate difficulty based on prev blocks
      - [x] append mined_block
      - [x] broadcast mined block
    - [x] handle incoming block proposal 
      - [x] suspend current block mining (node.mining)
      - [x] validate block
      - [x] broadcast proposed block (if valid)
  - [ ] state 
    - [x] account global state (balance, nonce, storageRoot, codeHash)
    - [ ] add miner fee after block inclusion
    - [ ] reverse transactions if network choose another block
    - [ ] contract storage
  - fixes
    - [ ] "Lwt_mvar.take node.blockchain" -> use a mutex instead
    - [ ] optimize block broadcasting (a lot of repeated requests)
    - [x] validate transaction
    - [ ] singleton Secp256k1 context
    - [ ] broadcast transactions?
    - [ ] gossip protocol
    - [ ] paginate /chain endpoint
    - [x] add transactions.hash field
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
  global_state: MKPTrie.trie Lwt_mvar.t;
  known_peers: string list Lwt_mvar.t;
  blocks_to_broadcast: Block.block list Lwt_mvar.t;
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
        print_endline "tx valid";
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
    hash = json |> member "hash" |> to_string;
    sender = json |> member "sender" |> to_string;
    receiver = json |> member "receiver" |> to_string;
    amount = json |> member "amount" |> to_int;
    gas_limit = json |> member "gas_limit" |> to_int;
    gas_price = json |> member "gas_price" |> to_int;
    nonce = json |> member "nonce" |> to_int;
    payload = json |> member "payload" |> to_string;
    signature = json |> member "signature" |> to_string;
  }

let block_of_json json: Block.block =
  let open Yojson.Basic.Util in
  {
    index = json |> member "index" |> to_int;
    previous_hash = json |> member "previous_hash" |> to_string;
    timestamp = json |> member "timestamp" |> to_float;
    transactions = json |> member "transactions" |> to_list |> List.map transaction_of_json;
    miner = json |> member "miner" |> to_string;
    state_root = json |> member "state_root" |> to_string;
    nonce = json |> member "nonce" |> to_int;
    hash = json |> member "hash" |> to_string;
    difficulty = json |> member "difficulty" |> to_int;
  }

let handle_transaction_request (node: node) (body: string) =
  let json = Yojson.Basic.from_string body in
  let tx = transaction_of_json json in
  add_transaction node.transaction_pool tx

let broadcast_block peers_list block =
  let block_json = Block.block_to_json block |> Yojson.Basic.to_string in

  let timeout_duration = 5.0 in

  let broadcast_to_peer peer =
    Printf.printf "broadcasting to peer: %s\n" peer;
    let uri = Uri.of_string (peer ^ "/block") in
    let headers = Cohttp.Header.init_with "Content-Type" "application/json" in

    let request =
      let _ = Cohttp_lwt_unix.Client.post
        ~headers
        ~body:(Cohttp_lwt.Body.of_string block_json)
        uri in
      Lwt.return_unit
    in
     
    let timeout =
      let* () = Lwt_unix.sleep timeout_duration in
      Printf.printf "Timeout while broadcasting to %s\n" peer;
      Lwt.return_unit
    in

    Lwt.pick [request;timeout]
  in
  let* _ = Lwt_list.iter_p broadcast_to_peer peers_list in
  Lwt.return_unit

let rec broadcast_block_routine node =
  let time_delay = 6. in
  let* _ = Lwt_unix.sleep time_delay in
  let* blocks = Lwt_mvar.take node.blocks_to_broadcast in
  let* _ = Lwt_mvar.put node.blocks_to_broadcast [] in
  
  print_endline "looking for blocks to broadcast\n";

  if List.length blocks > 0 then (
    print_endline "broadcasting blocks routine\n";

    let* peers = Lwt_mvar.take node.known_peers in
    let* _ = Lwt_mvar.put node.known_peers peers in
    let* _ = Lwt_list.iter_p (fun block -> 
      broadcast_block peers block;
    ) blocks in
    broadcast_block_routine node
  ) else (
    broadcast_block_routine node
  )

let handle_block_proposal_request node body =
  let received_block = Yojson.Basic.from_string body |> block_of_json in

  let* curr_chain = Lwt_mvar.take node.blockchain in
  let* _ = Lwt_mvar.put node.blockchain curr_chain in

  let prev_block = List.hd curr_chain in

  (* TODO: validate the first transaction "coinbase" *)
  (* TODO: validate if the chain received is longer*)
  (* TODO: test sending a malicious block *)

  (* if received_block.index > prev_block.index+1 then *)
  (*     begin *)
  (*     (* get longer chain and discover wich block is safe (last block agreed on network and local) *) *)
  (*     (* validate received chain *) *)
  (*     (* apply received chain *) *)
  (*     (* remove trasactions from mempool *) *)
  (*     print_endline "\n\n\nA OUTRA CHAIN É MAIOR\n\n\n"; *)
  (*     Lwt.return_false *)
  (*   end *)
  (* else if Block.validate_block received_block prev_block then ( *)
  if Block.validate_block received_block prev_block then (
    try
      (* List.iter (fun tx ->  *)
      (*   if not (Transaction.validate_transaction tx) then  *)
      (*     raise (Failure "Invalid transaction found on block") *)
      (* ) received_block.transactions; *)

      print_endline "appending received block";

      let* transaction_pool = Lwt_mvar.take node.transaction_pool in
      let updated_transaction_pool = List.filter (fun (mem_tx, _) ->
        not (List.exists (fun block_tx -> block_tx.Transaction.hash = mem_tx.Transaction.hash) received_block.transactions)
      ) transaction_pool in
      let* _ = Lwt_mvar.put node.transaction_pool updated_transaction_pool in

      let new_chain = received_block :: curr_chain in
      let* _ = Lwt_mvar.take node.blockchain in
      let* _ = Lwt_mvar.put node.blockchain new_chain in

      Lwt.return_true
    with 
    | Failure msg ->
      print_endline msg;
      Lwt.return_false)
  else 
    Lwt.return_false 

let mine_block curr_state transactions prev_block difficulty miner_addr =
  let open Block in

  (* TODO: validate broadcasting response *)
  let coinbase_tx: Transaction.transaction = {
    hash = "";
    sender = "0";
    receiver = miner_addr;
    amount = 1;
    gas_limit = 0;
    gas_price = 0;
    nonce = 0;
    payload = "coinbase";
    signature = "";
  } in

  let rec apply_transactions state transactions = 
    (match transactions with
    | [] -> state
    | tx :: rest ->
        (match Account.apply_transaction state tx with
        | Ok new_state -> 
            Printf.printf "Transaction executed ok!\n"; 
            apply_transactions new_state rest
        | Error err -> 
            Printf.printf "Transaction execution error: %s\n" err;
            apply_transactions state rest))
  in

  let block_transactions = coinbase_tx :: transactions in
  let updated_state = apply_transactions curr_state block_transactions in
  let state_root = MKPTrie.hash updated_state in

  let rec mine nonce =
    let* _ = Lwt.pause () in
    let candidate_block = {
      index = prev_block.index + 1;
      previous_hash = prev_block.hash;
      timestamp = Unix.time ();
      transactions = block_transactions;
      miner = miner_addr;
      state_root = state_root;
      nonce = nonce;
      difficulty = difficulty;
      hash = ""
    } in
    let candidate_hash = hash_block { candidate_block with hash = "" } in 
    if is_valid_pow candidate_hash difficulty then 
      Lwt.return (updated_state, { candidate_block with hash = candidate_hash })
    else
      mine (nonce + 1)
    in mine 0

let mining_routine node = 
  let threshold = 0 in
  (* let time_delay = 10.0 in *)
  let time_delay = float_of_int (Random.int 10) in
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
    let* _ = Lwt_unix.sleep time_delay in

    let* curr_pool = Lwt_mvar.take node.transaction_pool in
    let validated_transactions, remaining_transactions = List.partition (fun (_, verified) -> verified) curr_pool in
    let* _ = Lwt_mvar.put node.transaction_pool remaining_transactions in

    if List.length validated_transactions >= threshold then (
      let transactions_to_mine = List.map fst validated_transactions in

      let* curr_blockchain = Lwt_mvar.take node.blockchain in
      let* _ = Lwt_mvar.put node.blockchain curr_blockchain in

      let* curr_global_state = Lwt_mvar.take node.global_state in
      let* _ = Lwt_mvar.put node.global_state curr_global_state in

      let prev_block = List.hd curr_blockchain in
      let difficulty = Block.calculate_difficulty prev_block in
      let miner_addr = node.miner_addr in

      let* (new_state, mined_block) = mine_block curr_global_state transactions_to_mine prev_block difficulty miner_addr in

      let new_chain = mined_block :: curr_blockchain in
      let* _ = Lwt_mvar.take node.blockchain in
      let* _ = Lwt_mvar.put node.blockchain new_chain in

      let* _ = Lwt_mvar.take node.global_state in
      let* _ = Lwt_mvar.put node.global_state new_state in
      
      let* peers_list = Lwt_mvar.take node.known_peers in
      let* _ = Lwt_mvar.put node.known_peers peers_list in

      let* _ = broadcast_block peers_list mined_block in
      aux ()
    ) else (
      print_endline "mine pass...\n";
      let* _ = Lwt_mvar.put node.transaction_pool curr_pool in
      aux ()
    )
  in
  aux ()

let http_server node =
  let cors_headers = Cohttp.Header.of_list [
    ("Access-Control-Allow-Origin", "*");
    ("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
    ("Access-Control-Allow-Headers", "Content-Type");
  ] in
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

        (* TODO: apply transactions on state *)
        (match result with
        | true -> (
          let block = Yojson.Basic.from_string body |> block_of_json in
          let* curr_blocks = Lwt_mvar.take node.blocks_to_broadcast in
          let all_blocks = block :: curr_blocks in
          let* _ = Lwt_mvar.put node.blocks_to_broadcast all_blocks in
          Server.respond_string ~status:`OK ~body:"Block received is valid" ())
        | false -> 
          Server.respond_string ~status:`Bad_request ~body:"Block received is invalid" ()
        )
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
    | (`GET, "/blocks") ->
        let uri = req |> Request.uri in
        let query = Uri.query uri in

        let max_blocks_per_request = 10 in
        let start_param = List.assoc_opt "start" query in
        let end_param = List.assoc_opt "end" query in

        let start_idx = match start_param with
          | Some [start_str] -> int_of_string_opt start_str |> Option.value ~default:0
          | _ -> 0
        in
        let end_idx = match end_param with
          | Some [end_str] -> int_of_string_opt end_str |> Option.value ~default:max_blocks_per_request
          | _ -> max_blocks_per_request
        in

        let* chain = Lwt_mvar.take node.blockchain in
        let* _ = Lwt_mvar.put node.blockchain chain in

        let start_idx = max 0 start_idx in
        let end_idx = min (List.length chain) end_idx in

        let blocks_in_range = List.filteri (fun i _ -> i >= start_idx && i < end_idx) chain in

        let chain_json_list = List.map (fun bl -> Block.block_to_json bl) blocks_in_range in
        let json_body = `List chain_json_list |> Yojson.Basic.to_string in

        Server.respond_string ~headers:cors_headers ~status:`OK ~body:json_body ()
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
      broadcast_block_routine node;
      http_server node
    ] in
  Lwt_main.run main_loop
