type t = {
  hash: string;
  sender: string;
  receiver: string;
  amount: int;
  gas_limit: int;
  gas_price: int;
  nonce: int;
  payload: string;
  signature: string;
}

val hash_transaction: t -> string
val validate_transaction: t -> bool
val validate_transaction_coinbase: t -> bool
val string_of_transaction: t -> string
val transaction_to_json: t -> Yojson.Basic.t
val transaction_of_json: Yojson.Basic.t -> t
