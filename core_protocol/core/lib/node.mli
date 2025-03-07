open State

type node = {
  address: string;
  miner_addr: string;
  known_peers: string list ref;
  known_peers_mutex: Lwt_mutex.t;
  transaction_pool: (Transaction.t * bool) list ref;
  transaction_pool_mutex: Lwt_mutex.t;
  blockchain: Block.t list ref;
  blockchain_mutex: Lwt_mutex.t;
  mining: bool ref;
  mining_mutex: Lwt_mutex.t;
  global_state: State.t ref;
  global_state_mutex: Lwt_mutex.t;
  receipt_state: State.t ref;
  receipt_state_mutex: Lwt_mutex.t;
  contract_state: State.t ref;
  contract_state_mutex: Lwt_mutex.t;
  debug_enabled: bool;
}

val new_node: string -> string -> string list -> bool -> node
val run_node: node -> unit
