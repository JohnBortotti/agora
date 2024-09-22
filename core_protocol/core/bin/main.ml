(*
TODO:
  - [x] node
  - [x] consensus (forks and conflicts)
  - [ ] vm, fees and contracts (deploy, state and execution)
  - [x] configs via .env
  - tests
    - [x] pow
    - [x] transaction
    - [x] block
    - [ ] state
        - [x] RLP
        - [ ] MKPTrie
        - [ ] Account
 *)

open Agora_core
open Agora_core.Node
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

  let genesis: Block.t = {
    index = 0;
    previous_hash = "";
    timestamp = 1726089622.000000;
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
    global_state = Lwt_mvar.create (State.init_state "/home/opam/db-data/global-state" (Unsigned.Size_t.of_int 1024));
    known_peers = Lwt_mvar.create known_peers_env;
  } in
  run_node node
