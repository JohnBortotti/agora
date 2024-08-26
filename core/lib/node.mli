open Transaction
open State

type node = {
  transaction_pool: (Transaction.transaction * bool) list Lwt_mvar.t;
  blockchain: Block.block list Lwt_mvar.t;
  miner_addr: string;
  global_state: MKPTrie.trie
}

val add_transaction: (Transaction.transaction * bool) list Lwt_mvar.t -> Transaction.transaction -> unit Lwt.t
val validate_transaction: Transaction.transaction -> bool
val validate_transaction_pool: node -> unit Lwt.t
val get_valid_transactions: (Transaction.transaction * bool) list Lwt_mvar.t -> Transaction.transaction list Lwt.t
val mine_block: Transaction.transaction list -> Block.block -> int -> string -> Block.block
val mining_routine: node -> 'a Lwt.t
val run_node: node -> unit
