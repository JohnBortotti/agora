open Digestif.SHA256
open Cryptokit

module Transaction = struct 
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

  let hash_transaction tx =
    let data_to_hash =
      tx.sender ^ tx.receiver ^ string_of_int tx.amount
      ^ string_of_int tx.gas_limit ^ string_of_int tx.gas_price
      ^ string_of_int tx.nonce ^ tx.payload
    in
    to_hex (digest_string data_to_hash)

  let sign_transaction tx key =
    let hashed_tx = hash_transaction tx in
    transform_string (Hexa.encode()) (RSA.sign key hashed_tx)

  let trim_signature tx signed_tx =
    let expected_length = String.length tx in
    let current_length = String.length signed_tx in
    String.sub signed_tx (current_length - expected_length) expected_length

  let verify_transaction_signature tx key =
    let hashed_tx = transform_string (Hexa.encode()) (hash_transaction tx) in
    let signed_tx = transform_string (Hexa.decode()) tx.signature in
    let decrypted_tx = RSA.unwrap_signature key signed_tx in
    let decrypted_tx_hex = transform_string (Hexa.encode()) decrypted_tx in
    let trimmed_decrypted_tx = trim_signature hashed_tx decrypted_tx_hex in
    string_equal hashed_tx trimmed_decrypted_tx

  let validate_transaction _tx = true

  let string_of_transaction tx =
    "sender: " ^ tx.sender ^ "\n" ^
    "receiver: " ^ tx.receiver ^ "\n" ^
    "amount: " ^ string_of_int tx.amount ^ "\n" ^
    "gas limit: " ^ string_of_int tx.gas_limit ^ "\n" ^
    "gas price: " ^ string_of_int tx.gas_price ^ "\n" ^
    "nonce: " ^ string_of_int tx.nonce ^ "\n" ^
    "payload: " ^ tx.payload ^ "\n" ^
    "signature: " ^ tx.signature

  let string_of_transaction_compact tx =
    tx.sender ^ tx.receiver ^ string_of_int tx.amount ^ string_of_int tx.gas_limit ^ 
    string_of_int tx.gas_price ^ string_of_int tx.nonce ^ tx.payload ^ tx.signature

  let transaction_to_json tx =
    `Assoc [
      ("sender", `String tx.sender);
      ("receiver", `String tx.receiver);
      ("amount", `Int tx.amount);
      ("gas_limit", `Int tx.gas_limit);
      ("gas_price", `Int tx.gas_price);
      ("nonce", `Int tx.nonce);
      ("payload", `String tx.payload);
      ("signature", `String tx.signature)
    ]

  let transaction_to_json_string tx = 
    transaction_to_json tx |> Yojson.Basic.to_string
end

module Block = struct
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

  let string_of_block block =
    let txs = List.map Transaction.transaction_to_json_string block.transactions |> String.concat ",\n " in
    Printf.sprintf "Block {\n  index: %d;\n  previous_hash: %s;\n  timestamp: %f;
    transactions: [\n  %s\n  ];\n miner: %s;\n nonce: %d;\n difficulty: %d;\n hash: %s\n}"
    block.index
    block.previous_hash
    block.timestamp
    txs
    block.miner
    block.nonce
    block.difficulty
    block.hash

  let block_to_json block =
    `Assoc [
      ("index", `Int block.index);
      ("previous_hash", `String block.previous_hash);
      ("timestamp", `Float block.timestamp);
      ("transactions", `List (List.map Transaction.transaction_to_json block.transactions));
      ("nonce", `Int block.nonce);
      ("hash", `String block.hash);
      ("difficulty", `Int block.difficulty)
    ]

  let block_to_json_string block =
    block_to_json block |> Yojson.Basic.to_string

  let hash_block block =
    let transactions_str = String.concat "" (List.map (fun tx ->
      Transaction.string_of_transaction_compact tx
    ) block.transactions) in
    let data = 
      Printf.sprintf "%d%s%f%s%s%d%d"
        block.index
        block.previous_hash
        block.timestamp
        transactions_str
        block.miner
        block.nonce
        block.difficulty
    in
    to_hex (digest_string data)

  let is_valid_pow hash difficulty =
    String.sub hash 0 difficulty = String.make difficulty '0'

  let calculate_difficulty prev_block =
    let target_block_time = 20.0 in
    let current_time = Unix.time () -. prev_block.timestamp in
    let adjustment_factor = 
      if current_time < target_block_time then 1.2
      else if current_time > target_block_time then 0.9
      else 1.1
    in
    int_of_float (float_of_int prev_block.difficulty *. adjustment_factor)

  let validate_block block prev_block =
    let expected_hash = hash_block block in
    let difficulty = calculate_difficulty prev_block in
    if block.hash <> expected_hash then 
      (print_endline "invalid block hash";
      false)
    else if block.previous_hash <> prev_block.hash then
      (print_endline "invalid previous_block hash";
      false)
    else if not (is_valid_pow block.hash difficulty) then
      (print_endline "invalid pow";
      false)
    else if block.index <> prev_block.index + 1 then
      (print_endline "invalid block index";
      false)
    else
      (print_endline "block is valid";
      true)
end
