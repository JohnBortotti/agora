(*
node implementation:
  - [ ] state 
    - [ ] account global state (funds, nonce, storageRoot, codeHash)
    - [ ] contract storage
  - [ ] main loop
    - [ ] handle incoming transactions -> validate and append on transaction_pool
    - [ ] handle incoming block proposal -> validate block and propagate it
    - [ ] compute nonce -> after N transactions or X ms
 *)
open Transaction

type node = {
  transaction_pool: transaction list
}

let add_transaction (node: node) (tx: transaction): node =
  (* todo: validate transaction [hash, double spend, etc] *)
  { transaction_pool = tx :: node.transaction_pool }
