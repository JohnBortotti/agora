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
    index = 0;
    previous_hash = "";
    timestamp = Unix.time ();
    transactions = [];
    miner = "0";
    nonce = 0;
    difficulty = 4;
    hash = "";
  } in
  let node = {
    transaction_pool = Lwt_mvar.create [];
    blockchain = Lwt_mvar.create [{ genesis with hash = Block.hash_block genesis }];
    miner_addr = "0x0123";
    global_state = None;
  } in
  run_node node
