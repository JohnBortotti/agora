open Alcotest
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

let test_hash_transaction () =
  let expected = 
    "8740e1d4de573d7d2b50e63200df43738851015939bf5af943adcfac93ec011d"
  in 
  let result = hash_transaction sample_transaction in
  check string "Hash matches expected value" expected result

let test_validate_transaction () =
  let result = validate_transaction sample_transaction in
  check bool "Transaction is valid" true result

let test_validate_tx_coinbase () =
  let result = validate_transaction_coinbase sample_coinbase in
  check bool "Coinbase transaction is valid" true result

let test_string_of_tx () =
  let expected = 
    "hash: 8740e1d4de573d7d2b50e63200df43738851015939bf5af943adcfac93ec011d\nsender: 04c6047f9441ed7d6d3045406e95c07cd85c778e4b8cef3ca7abac09b95c709ee51ae168fea63dc339a3c58419466ceaeef7f632653266d0e1236431a950cfe52a\nreceiver: sample_receiver\namount: 1000\ngas limit: 21000\ngas price: 50\nnonce: 1\npayload: sample_payload\nsignature: 3f85f9fd6f4a9a29e51c20de8b5a3136575cd7eb8d74d6e7c73b22bfa87a23107e5d622833929ab2ab30674b24df986d4496d80db793481943fa73495f712f3601"
  in
  let result = string_of_transaction sample_transaction in
  check string "Transaction string matches expected" expected result

let test_transaction_json () =
  let json = transaction_to_json sample_transaction in
  let result = transaction_of_json json in
  (check (of_pp Fmt.nop)) "Transaction matches after JSON conversion"
    sample_transaction result

let () =
  run "Transaction module tests" [
    "hash_transaction", [test_case "hash" `Quick test_hash_transaction];
    "validate_transaction", [test_case "validate tx" `Quick test_validate_transaction];
    "validate_coinbase", [test_case "validate_coinbase" `Quick test_validate_tx_coinbase];
    "string_of_transaction", [test_case "string_of_ts" `Quick test_string_of_tx];
    "transaction_json", [test_case "transaction_json" `Quick test_transaction_json];
  ]
