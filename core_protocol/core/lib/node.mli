open State

type node = {
  address: string;
  transaction_pool: (Transaction.t * bool) list Lwt_mvar.t;
  blockchain: Block.t list Lwt_mvar.t;
  mining: bool Lwt_mvar.t;
  miner_addr: string;
  global_state: State.t Lwt_mvar.t;
  known_peers: string list Lwt_mvar.t;
}

val run_node: node -> unit
