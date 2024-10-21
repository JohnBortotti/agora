open State

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

type event = {
  name: string;
  address: string;
  topics: string list;
  data: int list;
}
 
type receipt = {
  transaction_hash: string;
  status: bool;
  message: string;
  gas_used: int;
  events: event list;
  bloom_filter: string;
  contract_address: string option;
}

val hash_transaction: t -> string
val validate_transaction: t -> bool
val validate_transaction_coinbase: t -> bool
val string_of_transaction: t -> string
val transaction_to_json: t -> Yojson.Basic.t
val transaction_of_json: Yojson.Basic.t -> t

val encode_event: event -> RLP.t
val encode_receipt: receipt -> RLP.t
val string_of_receipt: receipt -> string
val event_of_json: Yojson.Basic.t -> event
