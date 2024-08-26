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

let () = 
  print_endline "running node...\n";

  let node = {
    transaction_pool = Lwt_mvar.create [];
    global_state = None;
  } in
  run_node node
