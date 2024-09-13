type t = {
  index: int;
  previous_hash: string;
  timestamp: float;
  transactions: Transaction.t list;
  miner: string;
  state_root: string;
  nonce: int;
  difficulty: int;
  hash: string;
}

type header = {
  index: int;
  previous_hash: string;
  timestamp: float;
  nonce: int;
  hash: string;
  difficulty: int;
}

val block_to_header: t -> header
val string_of_block: t -> string
val string_of_block_header: header -> string
val block_to_json: t -> Yojson.Basic.t
val block_of_json: Yojson.Basic.t -> t
val blocks_of_json: Yojson.Basic.t -> t list
val block_header_to_json: header -> Yojson.Basic.t
val block_header_of_json: Yojson.Basic.t -> header
val block_headers_of_json: Yojson.Basic.t -> header list
val block_to_json_string: t -> string
val hash_block: t -> string
val is_valid_pow: string -> int -> bool
val calculate_difficulty: t -> int
val validate_block_transactions: t -> bool
val validate_block: t -> t -> bool
