(*
TODO:
  - [ ] tests 
    - [ ] test state.ml
    - [ ] test transaction.ml
    - [x] test pow.ml
  - [ ] node
  - [ ] consensus (forks and conflicts)
  - [ ] vm and contracts
  - [ ] logging
  - [x] configs via .env
 *)

open Agora_core.Node
open Agora_core.Transaction
open Agora_core.State

let get_env_var var_name =
  try
    Some (Sys.getenv var_name)
  with
  | Not_found -> None

let () = 
  let node_addr = (match get_env_var "NODE_ADDR" with 
  | Some x -> x  
  | None -> failwith "env var 'NODE_ADDR' not found") in

  let miner_addr_env = (match get_env_var "MINER_ADDR" with
  | Some x -> x
  | None -> failwith "env var 'MINER_ADDR' not found") in

  let known_peers_env = (match get_env_var "KNOWN_PEERS" with
  | Some x -> String.split_on_char ',' x
  | None -> failwith "env var 'KNOWN_PEERS' not found")
  in

  print_endline "running node...\n";

  let account_1: Account.t = {
    address = "0x000";
    balance = 2;
    nonce = 0;
    storage_root = "";
    code_hash = "";
  } in 
  
  let account_2: Account.t = {
    address = "0x002";
    balance = 50;
    nonce = 1;
    storage_root = "";
    code_hash = "";
  } in  

  let account_2_2: Account.t = {
    address = "0x002";
    balance = 60;
    nonce = 1;
    storage_root = "";
    code_hash = "";
  } in

  let global_state: MKPTrie.trie = None 
   |> fun f -> MKPTrie.insert f account_1.address (Account.encode account_1)
   |> fun f -> MKPTrie.insert f account_2.address (Account.encode account_2)
   |> fun f -> MKPTrie.insert f account_2.address (Account.encode account_2_2)
  in
  
  print_endline (MKPTrie.string_of_node global_state 0);

  let genesis: Block.block = {
    index = 0;
    previous_hash = "";
    timestamp = Unix.time ();
    transactions = [];
    miner = "0";
    state_root = "0";
    nonce = 0;
    difficulty = 4;
    hash = "";
  } in
  let node = {
    address = node_addr;
    transaction_pool = Lwt_mvar.create [];
    blockchain = Lwt_mvar.create [{ genesis with hash = Block.hash_block genesis }];
    mining = Lwt_mvar.create true;
    miner_addr = miner_addr_env;
    global_state = None;
    known_peers = Lwt_mvar.create known_peers_env;
    blocks_to_broadcast = Lwt_mvar.create [];
  } in
  run_node node
