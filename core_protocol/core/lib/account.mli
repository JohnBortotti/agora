open State 

module Account : sig
  type t = {
    address: string;
    balance: int;
    nonce: int;
    storage_root: string;
    code_hash: string;
  }

  val string_of_account: t -> string
  val account_to_json: t -> Yojson.Basic.t
  val encode: t -> RLP.t
  val decode: RLP.t -> t

  val apply_block_transactions: string -> State.t -> State.t -> Transaction.t list -> (Transaction.t -> string) -> (State.t * State.t)
end
