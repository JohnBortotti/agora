open Transaction
open State

type node = {
  transaction_pool: (Transaction.transaction * bool) list Lwt_mvar.t;
  global_state: MKPTrie.trie
}

val add_transaction: (Transaction.transaction * bool) list Lwt_mvar.t -> Transaction.transaction -> unit Lwt.t
val validate_transaction: Transaction.transaction -> bool
(* val validate_transaction_pool: (transaction * bool) list Lwt_mvar.t -> unit Lwt.t *)
val get_valid_transactions: (Transaction.transaction * bool) list Lwt_mvar.t -> Transaction.transaction list Lwt.t
val run_node: node -> unit
