open Digestif.SHA256

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

let block_to_header (block: t) = {
  index = block.index;
  previous_hash = block.previous_hash;
  timestamp = block.timestamp;
  nonce = block.nonce;
  hash = block.hash;
  difficulty = block.difficulty;
}

let string_of_block block =
  let transaction_to_json_string tx =
    Transaction.transaction_to_json tx |> Yojson.Basic.to_string
  in
  let txs = List.map transaction_to_json_string block.transactions |> String.concat ",\n " in
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

let block_to_json (block: t) =
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

let block_header_to_json (header: header) =
  `Assoc [
    ("index", `Int header.index);
    ("previous_hash", `String header.previous_hash);
    ("timestamp", `Float header.timestamp);
    ("nonce", `Int header.nonce);
    ("hash", `String header.hash);
    ("difficulty", `Int header.difficulty)
  ]

let block_of_json json: t =
  let open Yojson.Basic.Util in
  {
    index = json |> member "index" |> to_int;
    previous_hash = json |> member "previous_hash" |> to_string;
    timestamp = json |> member "timestamp" |> to_float;
    transactions = json |> member "transactions" |> to_list |> List.map Transaction.transaction_of_json;
    miner = json |> member "miner" |> to_string;
    state_root = json |> member "state_root" |> to_string;
    nonce = json |> member "nonce" |> to_int;
    hash = json |> member "hash" |> to_string;
    difficulty = json |> member "difficulty" |> to_int;
  }

let block_header_of_json json: header =
  let open Yojson.Basic.Util in
  {
    index = json |> member "index" |> to_int;
    previous_hash = json |> member "previous_hash" |> to_string;
    timestamp = json |> member "timestamp" |> to_float;
    nonce = json |> member "nonce" |> to_int;
    difficulty = json |> member "difficulty" |> to_int;
    hash = json |> member "hash" |> to_string;
  }

let hash_block block =
  let open Transaction in
  let string_of_transaction_compact tx =
    tx.sender ^ tx.receiver ^ string_of_int tx.amount ^ string_of_int tx.gas_limit ^ 
    string_of_int tx.gas_price ^ string_of_int tx.nonce ^ tx.payload ^ tx.signature
  in

  let transactions_str = String.concat "" (List.map (fun tx ->
    string_of_transaction_compact tx
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

let calculate_difficulty (prev_block: t) =
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
    false
  else if block.previous_hash <> prev_block.hash then
    false
  else if not (is_valid_pow block.hash difficulty) then
    false
  else if block.index <> prev_block.index + 1 then
    false
  else
    begin
      if validate_block_transactions block then
        true
      else
        false
    end
