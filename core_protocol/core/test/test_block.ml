open Alcotest
open Agora_core.Block
open Agora_core.Transaction

let sample_transaction = {
  hash = "8740e1d4de573d7d2b50e63200df43738851015939bf5af943adcfac93ec011d";
  sender = "04c6047f9441ed7d6d3045406e95c07cd85c778e4b8cef3ca7abac09b95c709ee51ae168fea63dc339a3c58419466ceaeef7f632653266d0e1236431a950cfe52a";
  receiver = "sample_receiver";
  amount = 1000;
  gas_limit = 21000;
  gas_price = 50;
  nonce = 1;
  payload = "sample_payload";
  signature = "3f85f9fd6f4a9a29e51c20de8b5a3136575cd7eb8d74d6e7c73b22bfa87a23107e5d622833929ab2ab30674b24df986d4496d80db793481943fa73495f712f3601"
}

let sample_coinbase  = {
  hash = "";
  sender = "0";
  receiver = "0";
  amount = 1;
  gas_limit = 0;
  gas_price = 0;
  nonce = 0;
  payload = "coinbase";
  signature = "";
}

let sample_block = {
  index = 1;
  previous_hash = "00000000000000000000000000000000";
  timestamp = 0.1;
  transactions = [sample_transaction];
  miner = "sample_miner";
  state_root = "sample_state_root";
  nonce = 42;
  difficulty = 2;
  hash = "sample_block_hash";
}

let sample_header = {
  index = 1;
  previous_hash = "00000000000000000000000000000000";
  timestamp = 0.1; 
  nonce = 42;
  hash = "sample_block_hash";
  difficulty = 2;
}

let test_block_to_header () =
  let result = block_to_header sample_block in
  let expected = {sample_header with timestamp = result.timestamp} in
  (check (of_pp Fmt.nop)) "Header matches expected value" expected result

let test_string_of_block () =
  let result = string_of_block sample_block in
  let expected = "Block {\n  index: 1;\n  previous_hash: 00000000000000000000000000000000;\n  timestamp: 0.100000;\n  transactions: [\n  {\"hash\":\"8740e1d4de573d7d2b50e63200df43738851015939bf5af943adcfac93ec011d\",\"sender\":\"04c6047f9441ed7d6d3045406e95c07cd85c778e4b8cef3ca7abac09b95c709ee51ae168fea63dc339a3c58419466ceaeef7f632653266d0e1236431a950cfe52a\",\"receiver\":\"sample_receiver\",\"amount\":1000,\"gas_limit\":21000,\"gas_price\":50,\"nonce\":1,\"payload\":\"sample_payload\",\"signature\":\"3f85f9fd6f4a9a29e51c20de8b5a3136575cd7eb8d74d6e7c73b22bfa87a23107e5d622833929ab2ab30674b24df986d4496d80db793481943fa73495f712f3601\"}\n  ];\n miner: sample_miner;\n state_root: sample_state_root;\n nonce: 42;\n difficulty: 2;\n hash: sample_block_hash\n}" in
  (check string) "String of block matches expected" expected result

let test_string_of_block_header () =
  let result = string_of_block_header sample_header in
  let expected = "Block_header {\n  index: 1;\n  previous_hash: 00000000000000000000000000000000;\n  timestamp: 0.100000;\n  \n nonce: 42;\n difficulty: 2;\n hash: sample_block_hash\n}" in
  (check string) "String of block header matches expected" expected result

let test_block_json () =
  let json = block_to_json sample_block in
  let result = block_of_json json in
  (check (of_pp Fmt.nop)) "Block matches after JSON conversion" sample_block result

let test_block_header_json () =
  let json = block_header_to_json sample_header in
  let result = block_header_of_json json in
  (check (of_pp Fmt.nop)) "Header matches after JSON conversion" sample_header result

let test_hash_block () =
  let result = hash_block sample_block in
  let expected = "5fc542f7a510fc0e63e3b53188ac009b8d2657057e30c0cd219e87261d87ae6e" in
  (check string) "Block hash matches expected" expected result

let test_is_valid_pow () =
  let result = is_valid_pow "00001abc" 3 in
  (check bool) "POW is valid" true result

let test_calculate_difficulty () =
  let result = calculate_difficulty sample_block in
  let expected = 1 in
  (check int) "Difficulty matches expected" expected result

let test_validate_block_transactions () =
  let t_block = { sample_block with transactions=[sample_coinbase;sample_transaction]} in
  let result = validate_block_transactions t_block in
  (check bool) "Transactions are valid" true result

(* let test_validate_block () = *)
(*   let t_block = {  *)
(*     sample_block with *)
(*     transactions=[sample_coinbase;sample_transaction]; *)
(*     previous_hash="1efb16612dacbd80c8d1d45d0cc83ca2bdabcc2ed8369e392317ef867bc65081"; *)
(*   } in *)
(*   let result = validate_block t_block sample_block in *)
(*   (check bool) "Block is valid" true result *)

let () =
  run "Block Tests" [
    "block_to_header", [test_case "block_to_header" `Quick test_block_to_header];
    "string_of_block", [test_case "string_of_block" `Quick test_string_of_block];
    "string_of_header", [test_case "string_of_header" `Quick test_string_of_block_header];
    "block_json", [test_case "block_json functions" `Quick test_block_json];
    "header_json", [test_case "header_json functions" `Quick test_block_header_json];
    "hash_block", [test_case "hash_block" `Quick test_hash_block];
    "is_valid_pow", [test_case "is_valid_pow" `Quick test_is_valid_pow];
    "calculate_difficulty", [test_case "calculate_difficulty" `Quick test_calculate_difficulty];
    "validate_txs", [test_case "validate_transactions" `Quick test_validate_block_transactions];
    (* "validate_block", [test_case "validate_block" `Quick test_validate_block]; *)
  ]
