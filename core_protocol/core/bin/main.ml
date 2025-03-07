(*
TODO:
  - [ ] agora lang compiler
    - [ ] function dispatcher "ABI"
  - [ ] contracts
    - [ ] persistent storage
    - [ ] bloom filter to track events
  - [ ] node
    - [ ] endpoint to get events
    - [ ] fixes
      - [ ] optimize block broadcasting (get rid of cyclical requests)
      - [ ] singleton secp256k1 context
 *)

open Agora_core.Node

let get_env_var var_name =
  try
    Some (Sys.getenv var_name)
  with
  | Not_found -> None

(* debug flag to enable debug features *)
let debug_mode = 
  match get_env_var "AGORA_DEBUG" with
  | Some "true" -> true
  | _ -> false

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

  print_endline (if debug_mode then "[NODE][DEBUG-ENABLED] running node..." else "[NODE] running node...");

  let node = new_node node_addr miner_addr_env known_peers_env debug_mode in
  run_node node;
