open Digestif.SHA256
open Cryptokit

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

let hash_transaction (tx: transaction) : string =
  let data_to_hash =
    tx.sender ^ tx.receiver ^ string_of_int tx.amount
    ^ string_of_int tx.gas_limit ^ string_of_int tx.gas_price
    ^ string_of_int tx.nonce ^ tx.payload
  in
  to_hex (digest_string data_to_hash)

let sign_transaction (tx: transaction) (key: RSA.key) : string =
  let hashed_tx = hash_transaction tx in
  transform_string (Hexa.encode()) (RSA.sign key hashed_tx)

let trim_signature (tx: string) (signed_tx: string) : string =
  let expected_length = String.length tx in
  let current_length = String.length signed_tx in
  String.sub signed_tx (current_length - expected_length) expected_length

let verify_transaction (tx: transaction) (key: RSA.key) : bool =
  let hashed_tx = transform_string (Hexa.encode()) (hash_transaction tx) in
  let signed_tx = transform_string (Hexa.decode()) tx.signature in
  let decrypted_tx = RSA.unwrap_signature key signed_tx in
  let decrypted_tx_hex = transform_string (Hexa.encode()) decrypted_tx in
  let trimmed_decrypted_tx = trim_signature hashed_tx decrypted_tx_hex in
  string_equal hashed_tx trimmed_decrypted_tx

let string_of_transaction (tx: transaction) : string =
  "sender: " ^ tx.sender ^ "\n" ^
  "receiver: " ^ tx.receiver ^ "\n" ^
  "amount: " ^ string_of_int tx.amount ^ "\n" ^
  "gas limit: " ^ string_of_int tx.gas_limit ^ "\n" ^
  "gas price: " ^ string_of_int tx.gas_price ^ "\n" ^
  "nonce: " ^ string_of_int tx.nonce ^ "\n" ^
  "payload: " ^ tx.payload ^ "\n" ^
  "signature: " ^ tx.signature
