(*
TODO:
  - [x] write and sign transaction
    - [x] transaction.mli
    - [ ] test transaction.ml
  - [ ] tests state.ml
    - [ ] test RLP
    - [ ] test MKPT
  - [ ] node
    - [ ] finish
    - [ ] test
  - [ ] consensus (forks and conflicts)
  - [ ] vm and contracts
 *)

open Agora_core.Node
open Agora_core.Transaction

let () = 
  print_endline "running node...\n";
  let genesis: Block.block = {
    index = 1;
    previous_hash = "0";
    timestamp = 0.0;
    transactions = [];
    miner = "";
    nonce = 0;
    hash = "";
  } in
  let node = {
    transaction_pool = Lwt_mvar.create [];
    blockchain = Lwt_mvar.create [genesis];
    miner_addr = "0x0123";
    global_state = None;
  } in
  run_node node
