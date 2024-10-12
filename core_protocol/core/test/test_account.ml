open Alcotest
open Agora_core.Transaction
open Agora_core.State
open Agora_core.Account.Account

let sample_account = {
  address = "0x1234567890abcdef";
  balance = 1000;
  nonce = 1;
  storage_root = "root_hash";
  code_hash = "code_hash";
} 

let mkptrie_testable : MKPTrie.trie Alcotest.testable =
    Alcotest.of_pp Fmt.nop

let result_testable : (MKPTrie.trie, string) result Alcotest.testable =
    Alcotest.result mkptrie_testable Alcotest.string

let test_string_of_account () =
  let result = string_of_account sample_account in
  let expected = "Address: 0x1234567890abcdef\nBalance: 1000\nNonce: 1\nStorage Root: root_hash\nCode Hash: code_hash\n" in
  (check string) "String of account matches expected" expected result

let test_account_to_json () =
  let yojson_testable : Yojson.Basic.t Alcotest.testable =
    let pp = Yojson.Basic.pp in
    let equal = Yojson.Basic.equal in
    testable pp equal
  in
  let result = account_to_json sample_account in
  let expected_json = `Assoc [
    ("address", `String "0x1234567890abcdef");
    ("balance", `Int 1000);
    ("nonce", `Int 1);
    ("storage_root", `String "root_hash");
    ("code_hash", `String "code_hash")
  ] in
  (check yojson_testable) "JSON conversion matches expected" expected_json result

let test_encode () =
  let result = encode sample_account in
  let expected_rlp = 
    `List [
      `String "0x1234567890abcdef";
      `String (string_of_int 1000);
      `String (string_of_int 1);
      `String "root_hash";
      `String "code_hash"
    ]
  in
  (check (of_pp Fmt.nop)) "RLP encoding matches expected" expected_rlp result

let test_decode () =
  let rlp_data = 
    `List [
      `String "0x1234567890abcdef";
      `String (string_of_int 1000);
      `String (string_of_int 1);
      `String "root_hash";
      `String "code_hash"
    ]
  in
  let result = decode rlp_data in
  (check (of_pp Fmt.nop)) "Decoding matches original account" sample_account result

let test_apply_transaction () =
  let sender_acc = {
    address = "1";
    balance = 8;
    nonce = 1;
    storage_root = "root_hash";
    code_hash = "code_hash";
  } in

  let initial_trie = match MKPTrie.insert None sender_acc.address (encode sender_acc) with
    | Some trie -> trie
    | None -> failwith "Failed to insert account into trie"
  in

  let transaction = {
    hash = "";
    sender = "1";
    receiver = "456";
    amount = 4;
    gas_limit = 2100;
    gas_price = 50;
    nonce = 1;
    payload = "";
    signature = "";
  } in

  let updated_sender_acc = {
    sender_acc with
    balance = sender_acc.balance - transaction.amount;
    nonce = sender_acc.nonce + 1;
  } in

  let receiver_acc = {
    address = "456";
    balance = 4;
    nonce = 0;
    storage_root = "";
    code_hash = "";
  } in

  let expected_trie =
    match MKPTrie.insert 
      (MKPTrie.insert (Some initial_trie) updated_sender_acc.address (encode updated_sender_acc))
      receiver_acc.address (encode receiver_acc) with
    | Some trie -> Ok (Some trie)
    | None -> failwith "Failed to insert updated accounts into expected trie"
  in

  let result = apply_transaction (Some initial_trie) transaction in
  (check result_testable) "Transaction applied correctly" expected_trie result

let test_apply_transaction_coinbase () =
  let trie = None in
  let coinbase_tx = {
    hash = "";
    sender = "0";
    receiver = "223";
    amount = 1;
    gas_limit = 0;
    gas_price = 0;
    nonce = 0;
    payload = "coinbase";
    signature = "";
  } in

  let result = apply_transaction_coinbase trie coinbase_tx in
  
  let receiver_acc = {
    address = "223";
    balance = 1;
    nonce = 0;
    storage_root = "";
    code_hash = "";
  } in

  let expected_trie =
    match MKPTrie.insert 
      None
      receiver_acc.address (encode receiver_acc) with
    | Some trie -> Ok (Some trie)
    | None -> failwith "Failed to insert updated accounts into expected trie"
  in
  (check result_testable) "Coinbase transaction applied correctly" expected_trie result

let test_apply_block_transactions () =
  let block_transactions = [
    {
      hash = "";
      sender = "0";
      receiver = "223";
      amount = 1;
      gas_limit = 0;
      gas_price = 0;
      nonce = 0;
      payload = "coinbase";
      signature = "";
    };
    {
      hash = "";
      sender = "223";
      receiver = "456";
      amount = 1;
      gas_limit = 0;
      gas_price = 0;
      nonce = 0;
      payload = "";
      signature = "";
    };
  ] in

  let sender_acc = {
    address = "223";
    balance = 0;
    nonce = 1;
    storage_root = "";
    code_hash = "";
  } in

  let receiver_acc = {
    address = "456";
    balance = 1;
    nonce = 0;
    storage_root = "";
    code_hash = "";
  } in

  let expected_trie =
    match MKPTrie.insert 
      (MKPTrie.insert None sender_acc.address (encode sender_acc))
      receiver_acc.address (encode receiver_acc) with
    | Some trie -> Ok (Some trie)
    | None -> failwith "Failed to insert updated accounts into expected trie"
  in

  let result = apply_block_transactions None block_transactions in
  (check result_testable) "Block transactions applied to trie" expected_trie (Ok result)

let () =
  run "Account Tests" [
    "string_of_account", [test_case "string_of_account" `Quick test_string_of_account];
    "account_to_json", [test_case "account_to_json" `Quick test_account_to_json];
    "encode", [test_case "encode" `Quick test_encode];
    "decode", [test_case "decode" `Quick test_decode];
    "apply_transaction", [test_case "apply_transaction" `Quick test_apply_transaction];
    "apply_transaction_coinbase", [test_case "apply_transaction_coinbase" `Quick test_apply_transaction_coinbase];
    "apply_block_transactions", [test_case "apply_block_transactions" `Quick test_apply_block_transactions];
  ]
