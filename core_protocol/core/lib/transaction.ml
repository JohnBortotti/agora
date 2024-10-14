open Digestif.SHA256
open Secp256k1
open Bigarray

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
  address: string;
  topics: string list;
  data: string;
}

type transaction_result = Success | Failure

type receipt = {
  transaction_hash: string;
  result: transaction_result;
  message: string;
  gas_used: int;
  logs: event list;
  bloom_filter: string;
  contract_address: string option;
}

let hash_transaction tx =
  let data_to_hash =
    tx.sender ^ tx.receiver ^ string_of_int tx.amount
    ^ string_of_int tx.gas_limit ^ string_of_int tx.gas_price
    ^ string_of_int tx.nonce ^ tx.payload
  in
  to_hex (digest_string data_to_hash)

let string_to_bigarray str =
  let len = String.length str in
  let ba = Array1.create char c_layout len in
  for i = 0 to len - 1 do
    Array1.set ba i str.[i]
  done;
  ba

let hex_to_bytes hex_str =
  let length = String.length hex_str in
  if length mod 2 <> 0 then invalid_arg "Hex string must have an even length";
  let buf = Bytes.create (length / 2) in
  for i = 0 to (length / 2) - 1 do
    let byte = int_of_string ("0x" ^ String.sub hex_str (2 * i) 2) in
    Bytes.set buf i (char_of_int byte)
  done;
  Bytes.to_string buf

let validate_transaction tx =
  let ctx = Context.create [Context.Sign; Context.Verify] in
  let expected_hash = hash_transaction tx in

  if not (equal (digest_string tx.hash) (digest_string expected_hash)) then
    begin
      Printf.printf "Invalid transaction: hash mismatch\n";
      false
    end
  else
    begin
      let tx_bytes = hex_to_bytes (hash_transaction tx) in
      let tx_buffer = string_to_bigarray tx_bytes in
      let tx_msg = Sign.msg_of_bytes_exn ~pos:0 tx_buffer in

      let decoded_signature = hex_to_bytes tx.signature in
      let sign_buffer = string_to_bigarray decoded_signature in

      let recid = Char.code (Bigarray.Array1.get sign_buffer 64) in

      let recoverable_sign =
        try
          let sig_val = Sign.read_recoverable_exn ctx ~recid ~pos:0 sign_buffer in
          sig_val
        with exn ->
          Printf.printf "Invalid recoverable signature\n";
          raise exn
      in
      let recovered_pk =
        try
          let pub_key = Sign.recover_exn ctx ~signature:recoverable_sign ~msg:tx_msg in
          pub_key
        with exn ->
          Printf.printf "Public key recovery failed\n";
          raise exn
      in
      let sender_pk = 
        try 
          let decoded_sender = hex_to_bytes tx.sender in
          let sender_buffer = string_to_bigarray decoded_sender in

          let sender_pub_key = Key.read_pk_exn ctx ~pos:0 sender_buffer in

          sender_pub_key
          with exn ->
            Printf.printf "Public key from tx.sender recovery failed: %s\n"
              (Printexc.to_string exn);
            raise exn
      in
      if not (Key.equal recovered_pk sender_pk) then
         begin
            Printf.printf "Public key mismatch: transaction sender and recovered key differ\n";
            false
          end
      else
        match Sign.verify ctx ~pk:recovered_pk ~msg:tx_msg ~signature:recoverable_sign with
        | Ok true ->
          true
        | _ ->
          Printf.printf "Signature verification failed\n";
          false
    end

let validate_transaction_coinbase tx =
  if tx.hash <> "" then
    false
  else if tx.sender <> "0" then
    false
  else if tx.amount <> 5 then
    false
  else if tx.gas_limit <> 0 then
    false
  else if tx.gas_price <> 0 then
    false
  else if tx.nonce <> 0 then
    false
  else if tx.payload <> "coinbase" then
    false
  else 
    true

let string_of_transaction tx =
  "hash: " ^ tx.hash ^ "\n" ^      
  "sender: " ^ tx.sender ^ "\n" ^
  "receiver: " ^ tx.receiver ^ "\n" ^
  "amount: " ^ string_of_int tx.amount ^ "\n" ^
  "gas limit: " ^ string_of_int tx.gas_limit ^ "\n" ^
  "gas price: " ^ string_of_int tx.gas_price ^ "\n" ^
  "nonce: " ^ string_of_int tx.nonce ^ "\n" ^
  "payload: " ^ tx.payload ^ "\n" ^
  "signature: " ^ tx.signature

let transaction_to_json tx =
  `Assoc [
    ("hash", `String tx.hash);
    ("sender", `String tx.sender);
    ("receiver", `String tx.receiver);
    ("amount", `Int tx.amount);
    ("gas_limit", `Int tx.gas_limit);
    ("gas_price", `Int tx.gas_price);
    ("nonce", `Int tx.nonce);
    ("payload", `String tx.payload);
    ("signature", `String tx.signature)
  ]

let transaction_of_json json: t =
  let open Yojson.Basic.Util in
  {
    hash = json |> member "hash" |> to_string;
    sender = json |> member "sender" |> to_string;
    receiver = json |> member "receiver" |> to_string;
    amount = json |> member "amount" |> to_int;
    gas_limit = json |> member "gas_limit" |> to_int;
    gas_price = json |> member "gas_price" |> to_int;
    nonce = json |> member "nonce" |> to_int;
    payload = json |> member "payload" |> to_string;
    signature = json |> member "signature" |> to_string;
  }

  let encode_event log =
    `List [
      `String log.address;
      `List (List.map (fun topic -> `String topic) log.topics);
      `String log.data
    ]

  let encode_receipt receipt =
    `List [
      `String receipt.transaction_hash;
      `String (match receipt.result with Success -> "Success" | Failure -> "Failure");
      `String (string_of_int receipt.gas_used);
      `List (List.map encode_event receipt.logs);
      `String receipt.bloom_filter;
      `String (match receipt.contract_address with None -> "" | Some addr -> addr)
    ]
