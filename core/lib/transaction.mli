module Transaction : sig
  type address = string

  type nonce = int

  type transaction = {
    hash: string;
    sender: address;
    receiver: address;
    amount: int;
    gas_limit: int;
    gas_price: int;
    nonce: int;
    payload: string;
    signature: string;
  }

  val hash_transaction: transaction -> string
  val validate_transaction: transaction -> bool
  val string_of_transaction: transaction -> string
  val string_of_transaction_compact: transaction -> string
  val transaction_to_json: transaction -> Yojson.Basic.t
  val transaction_to_json_string: transaction -> string
end

module Block : sig
  type block = {
    index: int;
    previous_hash: string;
    timestamp: float;
    transactions: Transaction.transaction list;
    miner: string;
    state_root: string;
    nonce: int;
    difficulty: int;
    hash: string;
  }

  type block_header = {
    index: int;
    previous_hash: string;
    timestamp: float;
    nonce: int;
    hash: string;
    difficulty: int;
  }

  val string_of_block: block -> string
  val hash_block: block -> string
  val is_valid_pow: string -> int -> bool
  val calculate_difficulty: block -> int
  val validate_block: block -> block -> bool
  val block_to_json: block -> Yojson.Basic.t
  val block_to_json_string: block -> string
  val block_to_header: block -> block_header
  val block_header_to_json: block_header -> Yojson.Basic.t
end
