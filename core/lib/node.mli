open Transaction
open State

type node = {
  transaction_pool: (transaction * bool) list Lwt_mvar.t;
  global_state: MKPTrie.trie
}

val add_transaction: (transaction * bool) list Lwt_mvar.t -> transaction -> unit Lwt.t
val validate_transaction: transaction -> bool
val validate_transaction_pool: (transaction * bool) list Lwt_mvar.t -> unit Lwt.t
val get_valid_transactions: (transaction * bool) list Lwt_mvar.t -> transaction list Lwt.t
val run_node: node -> unit
