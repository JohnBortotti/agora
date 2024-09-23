(*
TODO:
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
    - [x] add miner fee after block inclusion
    - [x] apply coinbase transaction on state
    - [x] front-end account explorer
    - [x] front-ent account consensus
    - [x] reverse transactions if network choose another block
    - [x] tool to generate transactions (wallet)
    - [x] re-broadcast incoming blocks
    - [ ] consensus -> choose "most work chain"
  - fixes
    - [ ] "Lwt_mvar.take node.blockchain" -> use a mutex instead
    - [ ] optimize block broadcasting (a lot of repeated requests)
    - [x] validate transaction
    - [ ] singleton Secp256k1 context
    - [ ] gossip protocol
    - [x] paginate /chain endpoint
    - [x] add transactions.hash field

notes:
  - consider that timeouts before mining can be useful, since the peer can receive transactions 
  in that interval, resulting in a longer chain resolution compared to nodes that start mining
  instantly, winning by "most work chain". also, implement the "most work chain" algorithm
 *)

open Transaction
open State
open Lwt
open Lwt.Syntax
open Cohttp_lwt_unix

type node = {
  address: string;
  transaction_pool: (Transaction.t * bool) list Lwt_mvar.t;
  blockchain: Block.t list Lwt_mvar.t;
  mining: bool Lwt_mvar.t;
  miner_addr: string;
  global_state: State.t Lwt_mvar.t;
  known_peers: string list Lwt_mvar.t;
}

let add_transaction pool tx =
  let* current_pool = Lwt_mvar.take pool in
  let updated_pool = (tx, false) :: current_pool in
  Lwt_mvar.put pool updated_pool
  
let rec validate_transaction_pool node =
  let* current_pool = Lwt_mvar.take node.transaction_pool in
  let new_pool = List.filter_map (fun (tx, verified) -> 
    if not verified then 
      begin
        if Transaction.validate_transaction tx then Some (tx, true)
        else None
      end
    else Some (tx, verified)
  ) current_pool in
  let* _ = Lwt_mvar.put node.transaction_pool new_pool in
  let* _ = Lwt_unix.sleep 3.0 in
  validate_transaction_pool node

let handle_transaction_request (node: node) (body: string) =
  let json = Yojson.Basic.from_string body in
  let tx = transaction_of_json json in
  add_transaction node.transaction_pool tx

let broadcast_block peers_list block =
  let peer_addr = Sys.getenv "NODE_ADDR" in

  let block_json = Block.block_to_json block |> Yojson.Basic.to_string in
  let payload = `Assoc [
    ("peer_addr", `String peer_addr);
    ("block", Yojson.Basic.from_string block_json)
  ] |> Yojson.Basic.to_string in

  let timeout_duration = 5.0 in

  let broadcast_to_peer peer =
    let uri = Uri.of_string (peer ^ "/block") in
    let headers = Cohttp.Header.init_with "Content-Type" "application/json" in

    let request =
      let _ = Cohttp_lwt_unix.Client.post
        ~headers
        ~body:(Cohttp_lwt.Body.of_string payload)
        uri in
      Lwt.return_unit
    in
     
    let timeout =
      let* () = Lwt_unix.sleep timeout_duration in
      Lwt.return_unit
    in

    Lwt.pick [request;timeout]
  in
  let* _ = Lwt_list.iter_p broadcast_to_peer peers_list in
  Lwt.return_unit

let handle_block_proposal_request node peer_addr body =
  let open Block in
  
  let received_block = body |> block_of_json in

  let* curr_chain = Lwt_mvar.take node.blockchain in
  let* _ = Lwt_mvar.put node.blockchain curr_chain in

  let prev_block = List.hd curr_chain in

  if received_block.index > prev_block.index+1 then
      begin
        print_endline "receiving longer chain\n";

        let block_end = string_of_int (received_block.index) in
        let block_start = string_of_int (max 0 (prev_block.index - 2)) in
        let uri = Uri.of_string
          (peer_addr ^ "/headers?start=" ^ block_start ^ "&end=" ^ block_end) 
        in

        Printf.printf "\ngetting headers: %s to %s\n\n" block_end block_start; 

        let* _, body = Cohttp_lwt_unix.Client.get uri in
        let* headers_json = Cohttp_lwt.Body.to_string body in
        let peer_headers = 
          Yojson.Basic.from_string headers_json 
          |> Yojson.Basic.Util.to_list |> List.map block_header_of_json 
        in

        (* TODO: sometimes the common block is older than fetched headers interval *)
        let rec find_last_common_block 
          (chain: Block.t list) (headers: Block.header list) =
          match chain, headers with
            | [], _ | _, [] -> None
            | block :: chain_rest, header :: headers_rest ->
              if block.index <> header.index then
                begin
                  find_last_common_block (block :: chain_rest) headers_rest
                end
              else
                begin
                  if block.hash = header.hash then Some block
                  else find_last_common_block chain_rest headers_rest
                end
        in

        let last_common_block = find_last_common_block curr_chain peer_headers in
        match last_common_block with
        | None ->
          Printf.printf "no common block found with the peer chain, rejecting the block.";
          Lwt.return_false
        | Some common_block ->
          Printf.printf "common block found index: %d\n\n" common_block.index;

          let uri = Uri.of_string 
            (peer_addr ^ "/blocks?start=" ^ (string_of_int (common_block.index+1)) ^ "&end=" ^ block_end)
          in

          let* _, body = Cohttp_lwt_unix.Client.get uri in
          let* blocks_json = Cohttp_lwt.Body.to_string body in

          let new_blocks: Block.t list = 
            Yojson.Basic.from_string blocks_json 
            |> Yojson.Basic.Util.to_list |> List.map block_of_json 
            |> List.rev
          in

          let rec validate_new_blocks prev_block new_blocks =
            match new_blocks with
            | [] -> Ok ()
            | block :: rest ->
              if Block.validate_block block prev_block then
                begin
                  validate_new_blocks block rest
                end
              else
                Error ("Invalid block found: " ^ string_of_block block)
          in
          (match validate_new_blocks common_block new_blocks with
          | Ok () ->
            let current_trusted_chain = List.filter
              (fun (block: Block.t) -> block.index <= common_block.index)
              curr_chain
            in

            print_endline "all new blocks are valid. Updating local chain.\n";

            let new_chain = List.rev (List.append current_trusted_chain new_blocks) in
            let* _ = Lwt_mvar.take node.blockchain in
            let* _ = Lwt_mvar.put node.blockchain new_chain in

            let* peers_list = Lwt_mvar.take node.known_peers in
            let* _ = Lwt_mvar.put node.known_peers peers_list in
            let* _ = broadcast_block peers_list received_block in

            let all_incoming_transactions = List.flatten 
                (List.map (fun block -> block.transactions) 
                new_blocks) 
            in
            let* transaction_pool = Lwt_mvar.take node.transaction_pool in
            let updated_tx_pool = List.filter (fun (mem_tx, _) -> 
                not (List.exists (fun block_tx -> 
                  block_tx.Transaction.hash = mem_tx.Transaction.hash) 
                all_incoming_transactions)
              )
              transaction_pool in
            let* _ = Lwt_mvar.put node.transaction_pool updated_tx_pool in

            let* curr_state = Lwt_mvar.take node.global_state in
            let reverted_state = State.revert_to_hash 
              curr_state 
              common_block.state_root 
            in
            let updated_trie = List.fold_left (fun state (block: Block.t) ->
                Account.apply_block_transactions state block.transactions
              )
              reverted_state.trie
              new_blocks 
            in

            let updated_state = {reverted_state with trie=updated_trie} in
            State.flush_to_db updated_state;
            let* _ = Lwt_mvar.put node.global_state updated_state in
            
            Lwt.return_true
          | Error msg ->
            print_endline msg;
            Lwt.return_false)
    end
  else if Block.validate_block received_block prev_block then 
    try
      print_endline "received block is valid\n";

      let* transaction_pool = Lwt_mvar.take node.transaction_pool in
      let updated_transaction_pool = List.filter (fun (mem_tx, _) ->
        not (List.exists (fun block_tx -> block_tx.Transaction.hash = mem_tx.Transaction.hash)
        received_block.transactions)
      ) transaction_pool in
      let* _ = Lwt_mvar.put node.transaction_pool updated_transaction_pool in

      let* curr_state = Lwt_mvar.take node.global_state in
      let updated_trie = Account.apply_block_transactions
        curr_state.trie
        received_block.transactions
      in  
      let updated_state = { curr_state with trie=updated_trie } in
      let _ = State.flush_to_db updated_state in
      let* _ = Lwt_mvar.put node.global_state updated_state in

      let new_chain = received_block :: curr_chain in
      let* _ = Lwt_mvar.take node.blockchain in
      let* _ = Lwt_mvar.put node.blockchain new_chain in

      let* peers_list = Lwt_mvar.take node.known_peers in
      let* _ = Lwt_mvar.put node.known_peers peers_list in
      let* _ = broadcast_block peers_list received_block in

      Lwt.return_true
    with 
    | Failure msg ->
      print_endline msg;
      Lwt.return_false
  else 
    Lwt.return_false 

let mine_block curr_state transactions (prev_block: Block.t) difficulty miner_addr  =
  let open Block in

  let coinbase_tx: Transaction.t = {
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

  let updated_state = Account.apply_block_transactions 
    curr_state
    (coinbase_tx :: transactions)
  in
  let state_root = MKPTrie.hash updated_state in

  let rec mine nonce =
    let* _ = Lwt.pause () in
    let candidate_block = {
      index = prev_block.index + 1;
      previous_hash = prev_block.hash;
      timestamp = Unix.time ();
      transactions = (coinbase_tx :: transactions);
      miner = miner_addr;
      state_root = state_root;
      nonce = nonce;
      difficulty = difficulty;
      hash = ""
    } in
    let candidate_hash = hash_block { candidate_block with hash = "" } in 
    if is_valid_pow candidate_hash difficulty then 
      (print_endline ("mined block: " ^ (string_of_int candidate_block.index) ^ "\n");
      Lwt.return (updated_state, { candidate_block with hash = candidate_hash }))
    else
      mine (nonce + 1)
    in mine 0

let mining_routine node = 
  let threshold = 0 in
  Random.init (int_of_string(Sys.getenv "ENTROPY_POOL"));
  let rec aux () = 
    let time_delay = float_of_int ((Random.int 10) + 5) in
    let* mining = Lwt_mvar.take node.mining in
    if mining then
      (let* _ = Lwt_mvar.put node.mining mining in 
      loop time_delay  ())
    else
      (let* _ = Lwt_mvar.put node.mining mining in 
      let* _ = Lwt_unix.sleep time_delay in
      aux ())
  and
  loop delay () =
    let* _ = Lwt_unix.sleep delay in

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

      let* (new_state_trie, mined_block) = mine_block curr_global_state.trie transactions_to_mine prev_block difficulty miner_addr in

      let new_chain = mined_block :: curr_blockchain in
      let* _ = Lwt_mvar.take node.blockchain in
      let* _ = Lwt_mvar.put node.blockchain new_chain in

      let* _ = Lwt_mvar.take node.global_state in
      let new_state = { curr_global_state with trie=new_state_trie } in
      let _ = State.flush_to_db new_state in
      let* _ = Lwt_mvar.put node.global_state new_state in

      let* peers_list = Lwt_mvar.take node.known_peers in
      let* _ = Lwt_mvar.put node.known_peers peers_list in

      let* _ = broadcast_block peers_list mined_block in
      aux ()
    ) else (
      let* _ = Lwt_mvar.put node.transaction_pool curr_pool in
      aux ()
    )
  in
  aux ()

let http_server node =
  let open Cohttp in
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
        let* _ = Lwt_mvar.take node.mining in
        let* _ = Lwt_mvar.put node.mining false in

        let* body = body |> Cohttp_lwt.Body.to_string in
        let json_body = Yojson.Basic.from_string body in

        let peer_addr = 
          json_body 
          |> Yojson.Basic.Util.member "peer_addr" 
          |> Yojson.Basic.Util.to_string in

        let block_json = 
          json_body 
          |> Yojson.Basic.Util.member "block" in

        let* result = handle_block_proposal_request node peer_addr block_json in
        let* _ = Lwt_mvar.take node.mining in
        let* _ = Lwt_mvar.put node.mining true in

        (match result with
        | true -> (
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

        let start_param = List.assoc_opt "start" query in
        let end_param = List.assoc_opt "end" query in

        let start_idx = match start_param with
          | Some [start_str] -> int_of_string_opt start_str |> Option.value ~default:0
          | _ -> 0
        in
        let end_idx = match end_param with
          | Some [end_str] -> int_of_string_opt end_str |> Option.value ~default:(start_idx+20)
          | _ -> (start_idx+20)
        in

        let* chain = Lwt_mvar.take node.blockchain in
        let* _ = Lwt_mvar.put node.blockchain chain in

        let start_idx = max 0 start_idx in
        let end_idx = min (List.length chain) end_idx in

        let blocks_in_range = List.filter
          (fun (block: Block.t) -> block.index >= start_idx && block.index <= end_idx) 
          chain
        in

        let chain_json_list = List.map
          (fun bl -> Block.block_to_json bl)
          blocks_in_range 
        in

        let json_body = `List chain_json_list |> Yojson.Basic.to_string in

        Server.respond_string ~headers:cors_headers ~status:`OK ~body:json_body ()
    | (`GET, "/headers") ->
        let uri = req |> Request.uri in
        let query = Uri.query uri in

        let start_param = List.assoc_opt "start" query in
        let end_param = List.assoc_opt "end" query in

        let start_idx = match start_param with
          | Some [start_str] -> int_of_string_opt start_str |> Option.value ~default:0
          | _ -> 0
        in
        let end_idx = match end_param with
          | Some [end_str] -> int_of_string_opt end_str |> Option.value ~default:(start_idx+20)
          | _ -> (start_idx+20)
        in

        let* chain = Lwt_mvar.take node.blockchain in
        let* _ = Lwt_mvar.put node.blockchain chain in

        let start_idx = max 0 start_idx in
        let end_idx = min (List.length chain) end_idx in

        let blocks_in_range = List.filter
          (fun (block: Block.t) -> block.index >= start_idx && block.index <= end_idx) 
          chain
        in

        let headers_json_list = 
          List.map (fun b -> Block.block_to_header b |> Block.block_header_to_json)
          blocks_in_range 
        in

        let json_body = `List headers_json_list |> Yojson.Basic.to_string in

        Server.respond_string ~headers:cors_headers ~status:`OK ~body:json_body ()
    | (`GET, "/account") ->
        let uri = req |> Request.uri in
        let query = Uri.query uri in
        let account = List.assoc_opt "account" query in

        (match account with
        | Some [account_addr] -> 
            let* state = Lwt_mvar.take node.global_state in 
            let* _ = Lwt_mvar.put node.global_state state in
            (match State.trie_get state account_addr with
            | None -> 
                Server.respond_string 
                  ~headers:cors_headers 
                  ~status:`Not_found 
                  ~body:"Account not found" ()
            | Some node ->
                (match node with
                  | Leaf (_, acc_data) 
                  | Branch (_, Some(acc_data)) ->
                    let decoded_acc = Account.decode (RLP.decode acc_data) in
                    let acc_json = Account.account_to_json decoded_acc in
                    let json_body = Yojson.Basic.to_string acc_json in
                    Server.respond_string ~status:`OK ~body:json_body ()
                  | _ ->
                      Server.respond_string 
                      ~headers:cors_headers 
                      ~status:`Not_found 
                      ~body:"Account not found" ())
            )
        | _ -> 
          Server.respond_string 
            ~headers:cors_headers 
            ~status:`Bad_request 
            ~body:"Empty account address" ())
    | _ ->
        Server.respond_string ~status:`Not_found ~body:"Not found" ()
  in
  let server = Server.make ~callback () in
  Server.create ~mode:(`TCP (`Port 8080)) server

(* TODO: on peer start, check other peers chain to update the local state *)
let run_node node =
  let main_loop =
    Lwt.join [
      validate_transaction_pool node;
      mining_routine node;
      http_server node
    ] in
  Lwt_main.run main_loop
