open Cryptokit

module Transaction : sig
  type address = string

  type nonce = int

  type transaction = {
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
  val sign_transaction: transaction -> RSA.key -> string
  val trim_signature: string -> string -> string
  val verify_transaction_signature: transaction -> RSA.key -> bool
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
    nonce: int;
    difficulty: int;
    hash: string;
  }

  val string_of_block: block -> string
  val hash_block: block -> string
  val is_valid_pow: string -> int -> bool
  val calculate_difficulty: block -> int
  val validate_block: block -> block -> bool
  val block_to_json: block -> Yojson.Basic.t
  val block_to_json_string: block -> string
end
