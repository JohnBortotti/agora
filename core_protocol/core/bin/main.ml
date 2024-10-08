(*
TODO:
  - [x] node
  - [x] consensus (forks and conflicts)
  - [x] JsonRPC (single request)
  - [ ] JsonRPC (batch request)
  - [ ] smart contracts
    - [ ] VM
    - [ ] agora lang
    - [ ] deploy contracts
    - [ ] contract execution
  - [ ] Transaction fees
  - [x] configs via .env
  - [ ] tests
    - [x] pow
    - [x] transaction
    - [x] block
    - [x] state
        - [x] RLP
        - [x] MKPTrie
        - [x] Account
        - [ ] node
        - [ ] jsonrpc
 *)

open Agora_core.Node

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

  print_endline "\nrunning node...\n";

  let node = new_node node_addr miner_addr_env known_peers_env in
  run_node node;
