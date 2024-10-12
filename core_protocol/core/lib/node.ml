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
  - [x] state 
    - [x] account global state (balance, nonce, storageRoot, codeHash)
    - [x] add miner fee after block inclusion
    - [x] apply coinbase transaction on state
    - [x] front-end account explorer
    - [x] front-ent account consensus
    - [x] reverse transactions if network choose another block
    - [x] tool to generate transactions (wallet)
    - [x] re-broadcast incoming blocks
  - [ ] fixes
    - [x] "Lwt_mvar.take node.blockchain" -> use a mutex instead
    - [ ] optimize block broadcasting (a lot of repeated requests)
    - [x] validate transaction
    - [ ] singleton Secp256k1 context
    - [ ] gossip protocol
    - [x] paginate /chain endpoint
    - [x] add transactions.hash field
 *)

open Transaction
open State
open Lwt.Syntax
open Virtual_machine
open Jsonrpc
open Account

type node = {
  address: string;
  miner_addr: string;
  known_peers: string list ref;
  known_peers_mutex: Lwt_mutex.t;
  transaction_pool: (Transaction.t * bool) list ref;
  transaction_pool_mutex: Lwt_mutex.t;
  blockchain: Block.t list ref;
  blockchain_mutex: Lwt_mutex.t;
  mining: bool ref;
  mining_mutex: Lwt_mutex.t;
  global_state: State.t ref;
  global_state_mutex: Lwt_mutex.t;
  receipt_state: State.t ref;
  receipt_state_mutex: Lwt_mutex.t;
  contract_state: State.t ref;
  contract_state_mutex: Lwt_mutex.t;
}

module Features = struct

  let add_transaction node tx =
    let* () = Lwt_mutex.lock node.transaction_pool_mutex in
    node.transaction_pool := (tx, false) :: !(node.transaction_pool);
    Lwt_mutex.unlock node.transaction_pool_mutex;
    Lwt.return_unit

  let get_account node address =
    let* _ = Lwt_mutex.lock node.global_state_mutex in
    let global_state = !(node.global_state) in
    Lwt_mutex.unlock node.global_state_mutex;

    match Account.get_account global_state.trie address with
      | Some acc_data ->
          Lwt_mutex.unlock node.global_state_mutex;
          let acc_json = Account.account_to_json acc_data in
          Lwt.return acc_json
      | _ -> 
          Lwt_mutex.unlock node.global_state_mutex;
          Lwt.return (`Assoc [ ("error", `String "Account not found") ])

  let get_code node address =
    let* () = Lwt_mutex.lock node.global_state_mutex in
    let global_state = !(node.global_state) in
    Lwt_mutex.unlock node.global_state_mutex;

    match Account.get_account global_state.trie address with
    | Some contract_acc_data ->
        let* () = Lwt_mutex.lock node.contract_state_mutex in
        let contract_state = !(node.contract_state) in
        Lwt_mutex.unlock node.contract_state_mutex;

        let contract_state = State.get_from_db contract_state contract_acc_data.code_hash in
        (match contract_state with
        | None -> Lwt.return (`Assoc [ ("error", `String "Contract code not found") ])
        | Some (`String code) -> Lwt.return (`Assoc [ ("code", `String code) ])
        | _ -> failwith "Invalid RLP encoding for contract code")
    | _ -> Lwt.return (`Assoc [ ("error", `String "Account not found") ])
    

  let get_block_headers node start_idx end_idx =
    let* () = Lwt_mutex.lock node.blockchain_mutex in
    let chain = !(node.blockchain) in
    Lwt_mutex.unlock node.blockchain_mutex;
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
    Lwt.return (`List headers_json_list)

  let get_blocks node start_idx end_idx =
    let* () = Lwt_mutex.lock node.blockchain_mutex in
    let chain = !(node.blockchain) in
    Lwt_mutex.unlock node.blockchain_mutex;
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
    Lwt.return (`List chain_json_list)

  let broadcast_block peers_list block =
    let peer_addr = Sys.getenv "NODE_ADDR" in

    let block_json = Block.block_to_json block in
    let timeout_duration = 5.0 in

    let broadcast_to_peer peer =
      let uri = Uri.of_string (peer ^ "/") in
      let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
      let json_rpc_request = Jsonrpc.create_request
        ~meth:"submit_block"
        ~params:(Some (`List [ `String peer_addr; block_json ]))
        ~id:(`Null)
      in
      let request_body = Jsonrpc.request_to_string json_rpc_request in
      let request =
        let* _ = Cohttp_lwt_unix.Client.post
          ~headers
          ~body:(Cohttp_lwt.Body.of_string request_body)
          uri in
        Lwt.return_unit
      in
      let timeout =
        let* () = Lwt_unix.sleep timeout_duration in
        Lwt.return_unit
      in
      Lwt.pick [request; timeout]
    in
    let* _ = Lwt_list.iter_p broadcast_to_peer peers_list in
    Lwt.return_unit

  let run_vm node tx = 
    let handle_method node method_name params =
      (match method_name with
      | "get_code" -> (
        (match params with 
          | Some (`List [ `String address ]) ->
              get_code node address
          | _ -> Lwt.return (create_error (-32602) "Invalid params" None))
      )
      | "get_headers" -> (
        (match params with
          | Some (`List [ `Int start_idx; `Int end_idx ]) ->
              get_block_headers node start_idx end_idx
          | _ -> Lwt.return (create_error (-32602) "Invalid params" None))
      )
      | "get_blocks" -> (
          (match params with
          | Some (`List [ `Int start_idx; `Int end_idx ]) ->
              get_blocks node start_idx end_idx
          | _ -> Lwt.return (create_error (-32602) "Invalid params" None))
      )
      | "get_account" -> (
          (match params with
          | Some (`List [ `String address ]) -> get_account node address
          | _ -> Lwt.return (create_error (-32602) "Invalid params" None)))
      | _ -> Lwt.return (create_error (-32601) "Method not found" None))
    in
    let handle_vm_request node request_json =
      (match Jsonrpc.parse_request_from_string request_json with
      | Ok req -> (
          match Jsonrpc.validate_request req with
          | Ok validated_req ->
              let* response = handle_method node validated_req.method_name validated_req.params in
              Lwt.return (Jsonrpc.create_response ~result:(response) validated_req.id)
          | Error err -> Lwt.return err
        )
      | Error err -> Lwt.return err)
    in
    VM.execute_vm tx (handle_vm_request node)

  let handle_block_proposal node peer_addr block_json =
    let received_block = Block.block_of_json block_json in
    let* _ = Lwt_mutex.lock node.blockchain_mutex in
    let curr_chain = !(node.blockchain) in
    Lwt_mutex.unlock node.blockchain_mutex;
    let prev_block = List.hd curr_chain in

    if received_block.index > prev_block.index + 1 then
      begin
        print_endline "receiving longer chain\n";

        let start_idx = max 0 (prev_block.index - 10) in
        let end_idx = received_block.index in

        let json_rpc_request = Jsonrpc.create_request
            ~meth:"get_headers"
            ~params:(Some (`List [ `Int start_idx; `Int end_idx ]))
            ~id:(`Int 1)
        in
        let request_body = Jsonrpc.request_to_string json_rpc_request in

        let uri = Uri.of_string (peer_addr ^ "/") in
        let headers = Cohttp.Header.init_with "Content-Type" "application/json" in

        Printf.printf "getting headers: %d to %d\n\n" end_idx start_idx; 

        let* _, body = Cohttp_lwt_unix.Client.post
            ~headers
            ~body:(Cohttp_lwt.Body.of_string request_body)
            uri
        in

        let* response_body = Cohttp_lwt.Body.to_string body in

        match Jsonrpc.parse_response_from_string response_body with
        | Ok { result = Some (`List headers_json_list); _ } ->
            let peer_headers = List.map Block.block_header_of_json headers_json_list in

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
            (match last_common_block with
            | None ->
              Printf.printf "no common block found with the peer chain, rejecting the block.";
              Lwt.return_false
            | Some common_block ->
              Printf.printf "common block found index: %d\n\n" common_block.index;

              let start_idx = common_block.index + 1 in
              let end_idx = received_block.index in

              let json_rpc_request = Jsonrpc.create_request
                  ~meth:"get_blocks"
                  ~params:(Some (`List [ `Int start_idx; `Int end_idx ]))
                  ~id:(`Int 2)
              in
              let request_body = Jsonrpc.request_to_string json_rpc_request in

              let uri = Uri.of_string (peer_addr ^ "/") in
              let headers = Cohttp.Header.init_with "Content-Type" "application/json" in

              let* _, body = Cohttp_lwt_unix.Client.post
                  ~headers
                  ~body:(Cohttp_lwt.Body.of_string request_body)
                  uri
              in

              let* response_body = Cohttp_lwt.Body.to_string body in

              match Jsonrpc.parse_response_from_string response_body with
              | Ok { result = Some (`List blocks_json_list); _ } ->
                  let new_blocks: Block.t list = 
                    List.map Block.block_of_json blocks_json_list
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
                        Error ("Invalid block found: " ^ Block.string_of_block block)
                  in
                  (match validate_new_blocks common_block new_blocks with
                  | Ok () ->
                    let current_trusted_chain = List.filter
                      (fun (block: Block.t) -> block.index <= common_block.index)
                      curr_chain
                    in

                    print_endline "all new blocks are valid. Updating local chain.\n";

                    let new_chain = List.rev (List.append current_trusted_chain new_blocks) in

                    let* _ = Lwt_mutex.lock node.blockchain_mutex in
                    node.blockchain := new_chain;
                    Lwt_mutex.unlock node.blockchain_mutex;

                    let* () = Lwt_mutex.lock node.known_peers_mutex in
                    let peers_list = !(node.known_peers) in
                    Lwt_mutex.unlock node.known_peers_mutex;
                    let* () = broadcast_block peers_list received_block in

                    let all_incoming_transactions = List.flatten 
                        (List.map (fun block -> block.Block.transactions) 
                        new_blocks) 
                    in

                    let* () = Lwt_mutex.lock node.transaction_pool_mutex in
                    let transaction_pool = !(node.transaction_pool) in
                    let updated_tx_pool = List.filter (fun (mem_tx, _) -> 
                        not (List.exists (fun block_tx -> 
                          block_tx.Transaction.hash = mem_tx.Transaction.hash) 
                        all_incoming_transactions)
                      )
                      transaction_pool in
                    node.transaction_pool := updated_tx_pool;
                    Lwt_mutex.unlock node.transaction_pool_mutex;

                    let* () = Lwt_mutex.lock node.global_state_mutex in
                    let curr_global_state = !(node.global_state) in
                    Lwt_mutex.unlock node.global_state_mutex;

                    let reverted_state = State.revert_to_hash 
                      !(node.global_state) 
                      common_block.state_root 
                    in

                    (* let* receipts_list = Lwt_list.map_p (fun (block: Block.t) ->
                      let receipts = Account.apply_block_transactions 
                      block.miner !(node.global_state) !(node.contract_state) 
                      block.transactions (run_vm node) in
                      Lwt.return receipts
                    ) new_blocks in *)

                    let* () = Lwt_mutex.lock node.global_state_mutex in
                    node.global_state := reverted_state;
                    Lwt_mutex.unlock node.global_state_mutex;

                    Lwt.return_true
                  | Error msg ->
                    print_endline msg;
                    Lwt.return_false)
              | Error _ ->
                  Lwt.return_false
              | _ ->
                  Lwt.return_false
            )
        | Error _ ->
            Lwt.return_false
        | _ ->
            Lwt.return_false
      end
    else if Block.validate_block received_block prev_block then 
      try
        print_endline "received block is valid\n";

        let* () = Lwt_mutex.lock node.transaction_pool_mutex in
        let transaction_pool = !(node.transaction_pool) in
        let updated_transaction_pool = List.filter (fun (mem_tx, _) ->
          not (List.exists (fun block_tx -> block_tx.Transaction.hash = mem_tx.Transaction.hash)
          received_block.transactions)
        ) transaction_pool in
        node.transaction_pool := updated_transaction_pool;
        Lwt_mutex.unlock node.transaction_pool_mutex;

        (* let* () = Lwt_mutex.lock node.global_state_mutex in *)
        (* let* () = Lwt_mutex.lock node.contract_state_mutex in *)

        (* let _ = Account.apply_block_transactions
          received_block.miner
          !(node.global_state)
          !(node.contract_state)
          received_block.transactions
          (run_vm node) in *)

        (* Lwt_mutex.unlock node.global_state_mutex; *)
        (* Lwt_mutex.unlock node.contract_state_mutex; *)

        let* () = Lwt_mutex.lock node.blockchain_mutex in
        node.blockchain := received_block :: curr_chain;
        Lwt_mutex.unlock node.blockchain_mutex;

        let* () = Lwt_mutex.lock node.known_peers_mutex in
        let peers_list = !(node.known_peers) in
        Lwt_mutex.unlock node.known_peers_mutex;
        let* () = broadcast_block peers_list received_block in

        Lwt.return_true
      with 
      | Failure msg ->
        print_endline msg;
        Lwt.return_false
    else 
      Lwt.return_false

  let rec validate_transaction_pool node =
    let* () = Lwt_mutex.lock node.transaction_pool_mutex in
    let current_pool = !(node.transaction_pool) in
    let new_pool = List.filter_map (fun (tx, verified) -> 
      if not verified then 
        begin
          if Transaction.validate_transaction tx then Some (tx, true)
          else None
        end
      else Some (tx, verified)
    ) current_pool in
    node.transaction_pool := new_pool;
    Lwt_mutex.unlock node.transaction_pool_mutex;
    let* _ = Lwt_unix.sleep 3.0 in
    let* _ = Lwt_unix.sleep 3.0 in
    validate_transaction_pool node

  let mine_block 
    node transactions (prev_block: Block.t) difficulty miner_addr  =
    let open Block in
    let coinbase_tx: Transaction.t = {
      hash = "";
      sender = "0";
      receiver = miner_addr;
      amount = 5;
      gas_limit = 0;
      gas_price = 0;
      nonce = 0;
      payload = "coinbase";
      signature = "";
    } in

    let* _ = Lwt_mutex.lock node.global_state_mutex in
    let global_state = !(node.global_state) in
    Lwt_mutex.unlock node.global_state_mutex;

    let* _ = Lwt_mutex.lock node.contract_state_mutex in
    let contract_state = !(node.contract_state) in
    Lwt_mutex.unlock node.contract_state_mutex;

    let* _ = Lwt_mutex.lock node.receipt_state_mutex in
    let receipt_state = !(node.receipt_state) in
    Lwt_mutex.unlock node.receipt_state_mutex;

    let (updated_state_trie, updated_contract_trie, updated_receipt_trie) = Account.apply_block_transactions
      node.miner_addr
      global_state.trie
      contract_state.trie
      receipt_state.trie
      (coinbase_tx :: transactions)
      (run_vm node)
    in  
    let state_root = MKPTrie.hash updated_state_trie in
    let receipt_root = MKPTrie.hash updated_receipt_trie in

    Printf.printf "global_state_root: %s\n" state_root;
    Printf.printf "receipt_root: %s\n" receipt_root;

    let rec mine nonce =
      let* _ = Lwt.pause () in
      let candidate_block = {
        index = prev_block.index + 1;
        previous_hash = prev_block.hash;
        timestamp = Unix.time ();
        transactions = (coinbase_tx :: transactions);
        miner = miner_addr;
        state_root = state_root;
        receipt_root = receipt_root;
        nonce = nonce;
        difficulty = difficulty;
        hash = ""
      } in
      let candidate_hash = hash_block { candidate_block with hash = "" } in 
      if is_valid_pow candidate_hash difficulty then 
        begin
          print_endline ("mined block: " ^ (string_of_int candidate_block.index) ^ "\n");
          Lwt.return (
            (updated_state_trie, updated_contract_trie, updated_receipt_trie),
            { candidate_block with hash = candidate_hash }
          )
        end
      else
        mine (nonce + 1)
      in mine 0

  let mining_routine node =
    let threshold = 0 in
    Random.init (int_of_string (Sys.getenv "ENTROPY_POOL"));
    let rec aux () =
      let time_delay = float_of_int ((Random.int 2) + 0) in
      let* () = Lwt_mutex.lock node.mining_mutex in
      let mining = !(node.mining) in
      Lwt_mutex.unlock node.mining_mutex;
      if mining then
        loop time_delay ()
      else
        let* () = Lwt_unix.sleep time_delay in
        aux ()
    and
    loop delay () =
      let* () = Lwt_unix.sleep delay in

      let* () = Lwt_mutex.lock node.transaction_pool_mutex in      
      let validated_transactions, remaining_transactions =
        List.partition (fun (_, verified) -> verified) !(node.transaction_pool) in
        node.transaction_pool := remaining_transactions;
      Lwt_mutex.unlock node.transaction_pool_mutex;

      if List.length validated_transactions >= threshold then (
        let transactions_to_mine = List.map fst validated_transactions in

        let curr_blockchain = !(node.blockchain) in

        let prev_block = List.hd curr_blockchain in
        let difficulty = Block.calculate_difficulty prev_block in
        let miner_addr = node.miner_addr in

        let* ((
          updated_state_trie, 
          updated_contract_trie, 
          updated_receipt_trie
        ), mined_block) = mine_block 
          node 
          transactions_to_mine prev_block difficulty miner_addr
        in

        (* update blockchain *)
        let* () = Lwt_mutex.lock node.blockchain_mutex in
        node.blockchain := mined_block :: curr_blockchain;
        Lwt_mutex.unlock node.blockchain_mutex;

        (* update global state *)
        let* () = Lwt_mutex.lock node.global_state_mutex in
        node.global_state := { 
          !(node.global_state) with trie = updated_state_trie;
        };
        State.flush_trie_to_db !(node.global_state);
        Lwt_mutex.unlock node.global_state_mutex;

        (* update contract state *)
        let* () = Lwt_mutex.lock node.contract_state_mutex in
        node.contract_state := {
          !(node.contract_state) with trie = updated_contract_trie;
        };
        State.flush_trie_to_db !(node.contract_state);
        Lwt_mutex.unlock node.contract_state_mutex;

        (* update receipt state *)
        let* () = Lwt_mutex.lock node.receipt_state_mutex in
        node.receipt_state := {
          !(node.receipt_state) with trie = updated_receipt_trie;
        };
        State.flush_trie_to_db !(node.receipt_state);
        Lwt_mutex.unlock node.receipt_state_mutex;

        let* () = Lwt_mutex.lock node.known_peers_mutex in
        let peers_list = !(node.known_peers) in
        Lwt_mutex.unlock node.known_peers_mutex;
        
        (* let* () = broadcast_block peers_list mined_block in *)
        aux ()
      ) else (
        aux ()
      )
    in
    aux ()
end

module RpcInterface = struct
  let call_method node method_name params =
    match method_name with
    | "submit_transaction" -> (
      match params with
        | Some (`List [ tx_json ]) ->
            let tx = Transaction.transaction_of_json tx_json in
            let* _ = Features.add_transaction node tx in
            Lwt.return (`String "Transaction received")
        | _ -> Lwt.return (create_error (-32602) "Invalid params" None)
    )
    | "submit_block" -> (
      match params with
        | Some (`List [ `String peer_addr; block_json ]) ->
            let* _ = Features.handle_block_proposal node peer_addr block_json in
            Lwt.return (`String "Block received")
        | _ -> Lwt.return (create_error (-32602) "Invalid params" None)
    )
    | "get_headers" -> (
      match params with
        | Some (`List [ `Int start_idx; `Int end_idx ]) ->
            Features.get_block_headers node start_idx end_idx
        | _ -> Lwt.return (create_error (-32602) "Invalid params" None)
    )
    | "get_blocks" -> (
        match params with
        | Some (`List [ `Int start_idx; `Int end_idx ]) ->
            Features.get_blocks node start_idx end_idx
        | _ -> Lwt.return (create_error (-32602) "Invalid params" None)
    )
    | "get_account" -> (
        match params with
        | Some (`List [ `String address ]) -> Features.get_account node address
        | _ -> Lwt.return (create_error (-32602) "Invalid params" None))
    | _ -> Lwt.return (create_error (-32601) "Method not found" None)

  let handle_request node request_json =
    (match Jsonrpc.parse_request_from_string request_json with
    | Ok req -> (
        match Jsonrpc.validate_request req with
        | Ok validated_req ->
            let* response = call_method node validated_req.method_name validated_req.params in
            Lwt.return (Jsonrpc.create_response ~result:(response) validated_req.id)
        | Error err -> Lwt.return err
      )
    | Error err -> Lwt.return err)
end

module Transport = struct
  open Cohttp_lwt_unix
  open Lwt.Infix
  open Cohttp

  let http_server node =
    Lwt_io.printf "listening for Http connections at: %s\n" node.address >>= fun () ->
    let cors_headers = Cohttp.Header.of_list [
      ("Access-Control-Allow-Origin", "*");
      ("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
      ("Access-Control-Allow-Headers", "Content-Type");
    ] in
    let callback _conn req body =
      let uri = req |> Request.uri |> Uri.path in
      match (Request.meth req, uri) with
      | (`POST, "/") ->
            body |> Cohttp_lwt.Body.to_string >>= fun body_string ->
            RpcInterface.handle_request node body_string >>= fun response_json ->
            let response_body = Jsonrpc.response_to_string response_json in
            Server.respond_string ~headers:cors_headers ~status:`OK ~body:response_body ()
      | _ ->
          Server.respond_string ~status:`Not_found ~body:"Not found" ()
    in
    let server = Server.make ~callback () in
    Server.create ~mode:(`TCP (`Port 8080)) server
end

let new_node node_addr miner_addr known_peers =
  let open Block in
  
  let genesis = {
    index = 0;
    previous_hash = "";
    timestamp = 1726089622.000000;
    transactions = [];
    miner = "0";
    state_root = "0";
    receipt_root = "0"; 
    nonce = 0;
    difficulty = 4;
    hash = "";
  } in
  {
    address = node_addr;
    miner_addr;
    transaction_pool = ref [];
    transaction_pool_mutex = Lwt_mutex.create ();
    blockchain = ref [{ genesis with hash = Block.hash_block genesis }];
    blockchain_mutex = Lwt_mutex.create ();
    mining = ref true;
    mining_mutex = Lwt_mutex.create ();
    global_state = ref (State.init_state "/home/opam/db-data/global-state" (Unsigned.Size_t.of_int 1024));
    global_state_mutex = Lwt_mutex.create ();
    contract_state = ref (State.init_state "/home/opam/db-data/contract-state" (Unsigned.Size_t.of_int 1024));
    contract_state_mutex = Lwt_mutex.create ();
    receipt_state = ref (State.init_state "/home/opam/db-data/receipt-state" (Unsigned.Size_t.of_int 1024));
    receipt_state_mutex = Lwt_mutex.create ();
    known_peers = ref known_peers;
    known_peers_mutex = Lwt_mutex.create ();
  }
   
let run_node node =
  let main_loop =
    Lwt.join [
      Features.validate_transaction_pool node;
      Features.mining_routine node;
      Transport.http_server node;
    ] in
  Lwt_main.run main_loop
