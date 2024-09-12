open Digestif.SHA256
open Secp256k1
open Bigarray

module Transaction = struct 
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

  let hex_of_bigarray buffer =
    let len = Bigarray.Array1.dim buffer in
    let hex_str = Buffer.create (len * 2) in
    for i = 0 to len - 1 do
      Buffer.add_string hex_str 
        (Printf.sprintf "%02x" (Char.code (Bigarray.Array1.get buffer i)))
    done;
    Buffer.contents hex_str

  let _bigarray_to_string buffer =
    let len = Bigarray.Array1.dim buffer in
    let str = Bytes.create len in
    for i = 0 to len - 1 do
      Bytes.set str i (Bigarray.Array1.get buffer i)
    done;
    Bytes.to_string str

  let validate_transaction tx =
    let ctx = Context.create [Context.Sign; Context.Verify] in
    let expected_hash = hash_transaction tx in
    Printf.printf "Expected hash: %s\n" expected_hash;
    Printf.printf "Transaction hash: %s\n" tx.hash;

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
        Printf.printf "Transaction message created successfully\n";

        let decoded_signature = hex_to_bytes tx.signature in
        let sign_buffer = string_to_bigarray decoded_signature in
        Printf.printf "Signature buffer created\n";

        let recid = Char.code (Bigarray.Array1.get sign_buffer 64) in
        Printf.printf "Recid: %d\n" recid;

        let recoverable_sign =
          try
            let sig_val = Sign.read_recoverable_exn ctx ~recid ~pos:0 sign_buffer in
            Printf.printf "Recoverable signature read successfully\n";
            let sign_hex = Sign.to_bytes ctx sig_val in
            Printf.printf "Signature %s\n" (hex_of_bigarray sign_hex);
            sig_val
          with exn ->
            Printf.printf "Invalid recoverable signature\n";
            raise exn
        in
        let recovered_pk =
          try
            let pub_key = Sign.recover_exn ctx ~signature:recoverable_sign ~msg:tx_msg in
            Printf.printf "Public key recovered successfully\n";
            let pub_key_bytes = Key.to_bytes ~compress:false ctx pub_key in
            Printf.printf "Recovered Public Key (hex): %s\n" (hex_of_bigarray pub_key_bytes);
            pub_key
          with exn ->
            Printf.printf "Public key recovery failed\n";
            raise exn
        in
        let sender_pk = 
          try 
            let decoded_sender = hex_to_bytes tx.sender in
            let sender_buffer = string_to_bigarray decoded_sender in
            Printf.printf "Sender public key converted to bigarray\n";

            let sender_pub_key = Key.read_pk_exn ctx ~pos:0 sender_buffer in
            let sender_pub_key_bytes = Key.to_bytes ~compress:false ctx sender_pub_key in

            let sender_pk_hex = hex_of_bigarray sender_pub_key_bytes in
            Printf.printf "Public key from tx.sender recovered successfully\n";
            Printf.printf "tx.sender Public Key (hex): %s\n" sender_pk_hex;
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
            Printf.printf "Signature verification succeeded\n";
            true
          | _ ->
            Printf.printf "Signature verification failed\n";
            false
      end

  (* TODO: validate coinbase tx signature? *)
  let validate_transaction_coinbase tx =
    if tx.hash <> "" then
      false
    else if tx.sender <> "0" then
      false
    else if tx.amount <> 1 then
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

  let string_of_transaction_compact tx =
    tx.sender ^ tx.receiver ^ string_of_int tx.amount ^ string_of_int tx.gas_limit ^ 
    string_of_int tx.gas_price ^ string_of_int tx.nonce ^ tx.payload ^ tx.signature

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

  let block_to_header (block: block): block_header = {
    index = block.index;
    previous_hash = block.previous_hash;
    timestamp = block.timestamp;
    nonce = block.nonce;
    hash = block.hash;
    difficulty = block.difficulty;
  }

  let string_of_block block =
    let txs = List.map Transaction.transaction_to_json_string block.transactions |> String.concat ",\n " in
    Printf.sprintf "Block {\n  index: %d;\n  previous_hash: %s;\n  timestamp: %f;
    transactions: [\n  %s\n  ];\n miner: %s;\n state_root: %s;\n nonce: %d;\n difficulty: %d;\n hash: %s\n}"
    block.index
    block.previous_hash
    block.timestamp
    txs
    block.miner
    block.state_root
    block.nonce
    block.difficulty
    block.hash

  let string_of_block_header block_header =
    Printf.sprintf "Block_header {\n  index: %d;\n  previous_hash: %s;\n  timestamp: %f;
    \n nonce: %d;\n difficulty: %d;\n hash: %s\n}"
    block_header.index
    block_header.previous_hash
    block_header.timestamp
    block_header.nonce
    block_header.difficulty
    block_header.hash

  let block_to_json (block: block) =
    `Assoc [
      ("index", `Int block.index);
      ("previous_hash", `String block.previous_hash);
      ("timestamp", `Float block.timestamp);
      ("transactions", `List (List.map Transaction.transaction_to_json block.transactions));
      ("nonce", `Int block.nonce);
      ("miner", `String block.miner);
      ("state_root", `String block.state_root);
      ("hash", `String block.hash);
      ("difficulty", `Int block.difficulty)
    ]

  let block_header_to_json (header: block_header) =
    `Assoc [
      ("index", `Int header.index);
      ("previous_hash", `String header.previous_hash);
      ("timestamp", `Float header.timestamp);
      ("nonce", `Int header.nonce);
      ("hash", `String header.hash);
      ("difficulty", `Int header.difficulty)
    ]

  let block_to_json_string block =
    block_to_json block |> Yojson.Basic.to_string

  let hash_block block =
    let transactions_str = String.concat "" (List.map (fun tx ->
      Transaction.string_of_transaction_compact tx
    ) block.transactions) in
    let data = 
      Printf.sprintf "%d%s%f%s%s%s%d%d"
        block.index
        block.previous_hash
        block.timestamp
        transactions_str
        block.miner
        block.state_root
        block.nonce
        block.difficulty
    in
    to_hex (digest_string data)

  let is_valid_pow hash difficulty =
    String.sub hash 0 difficulty = String.make difficulty '0'

  let calculate_difficulty (prev_block: block) =
    let target_block_time = 20.0 in
    let current_time = Unix.time () -. prev_block.timestamp in
    let adjustment_factor = 
      if current_time < target_block_time then 1.2
      else if current_time > target_block_time then 0.9
      else 1.1
    in
    int_of_float (float_of_int prev_block.difficulty *. adjustment_factor)

  let validate_block_transactions block = 
    match block.transactions with
    | [] -> 
      print_endline "block doesnt have coinbase transaction";
      false
    | coinbase :: rest ->
      if Transaction.validate_transaction_coinbase coinbase then
        begin
          let rec aux = function 
          | [] -> true
          | tx :: rest ->
              if Transaction.validate_transaction tx then
                aux rest
              else 
                false
          in
          aux rest
        end
      else 
        (print_endline "invalid coinbase transaction";
        false)

  let validate_block block prev_block =
    let expected_hash = hash_block block in
    let difficulty = calculate_difficulty prev_block in
    if block.hash <> expected_hash then 
      (print_endline "invalid block hash\n";
      false)
    else if block.previous_hash <> prev_block.hash then
      (print_endline "invalid previous_hash hash\n";
      false)
    else if not (is_valid_pow block.hash difficulty) then
      (print_endline "invalid pow\n";
      false)
    else if block.index <> prev_block.index + 1 then
      (print_endline "invalid block index\n";
      false)
    else
      begin
        print_endline "validating block transactions";
        if validate_block_transactions block then
          (print_endline "block is valid\n";
          true)
        else
          false
      end
end
