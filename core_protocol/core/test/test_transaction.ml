open Alcotest
open Agora_core.Transaction

let sample_transaction = {
  hash = "test";
  sender = "sample_sender";
  receiver = "sample_receiver";
  amount = 1000;
  gas_limit = 21000;
  gas_price = 50;
  nonce = 1;
  payload = "sample_payload";
  signature = "sample_signature";
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
    "977142d693a3cc3c58955f18189ff4be3f9b88f663035d1b187417b2c74c7f00"
  in 
  let result = hash_transaction sample_transaction in
  check string "Hash matches expected value" expected result

(* let test_validate_transaction () = *)
(*   let result = validate_transaction sample_transaction in *)
(*   check bool "Transaction is valid" true result *)

let test_validate_tx_coinbase () =
  let result = validate_transaction_coinbase sample_coinbase in
  check bool "Coinbase transaction is valid" true result

let test_string_of_tx () =
  let expected = 
    "hash: test\nsender: sample_sender\nreceiver: sample_receiver\namount: 1000\ngas limit: 21000\ngas price: 50\nnonce: 1\npayload: sample_payload\nsignature: sample_signature"
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
    (* "validate_transaction", [test_case "validate" `Quick test_validate_transaction]; *)
    "validate_coinbase", [test_case "validate_coinbase" `Quick test_validate_tx_coinbase];
    "string_of_transaction", [test_case "string_of_ts" `Quick test_string_of_tx];
    "transaction_json", [test_case "transaction_json" `Quick test_transaction_json];
  ]
