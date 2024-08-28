open Transaction
open State

type node = {
  address: string;
  transaction_pool: (Transaction.transaction * bool) list Lwt_mvar.t;
  blockchain: Block.block list Lwt_mvar.t;
  mining: bool Lwt_mvar.t;
  miner_addr: string;
  global_state: MKPTrie.trie;
  known_peers: string list Lwt_mvar.t;
}

val add_transaction: (Transaction.transaction * bool) list Lwt_mvar.t -> Transaction.transaction -> unit Lwt.t
val validate_transaction_pool: node -> unit Lwt.t
val mine_block: Transaction.transaction list -> Block.block -> int -> string -> Block.block
val mining_routine: node -> 'a Lwt.t
val run_node: node -> unit
