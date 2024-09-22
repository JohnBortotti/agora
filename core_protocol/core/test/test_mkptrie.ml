open Alcotest
open Agora_core.State.MKPTrie

let sample_leaf = insert None "a" (`String "test value")

let test_string_of_node () =
  let res1 = string_of_node sample_leaf 0 in
  let exp1 = "Leaf -> (key: 61, value: test value)" in
  (check string) "String of node matches expected" res1 exp1

let test_string_to_nibbles () =
  let result = string_to_nibbles "abc" in
  let expected = [6; 1; 6; 2; 6; 3] in
  (check (list int)) "String to nibbles matches expected" expected result

let test_nibbles_to_string () =
  let result = nibbles_to_string [6; 1; 6; 2; 6; 3] in
  let expected = "616263" in
  Alcotest.(check string) "Nibbles to string matches expected" expected result

let test_common_prefix_length () =
  let result = common_prefix_length ["abc"; "ab"; "abd"] ["abc"; "ab"; "abe"] in
  let expected = 2 in
  (check int) "Common prefix length matches expected" expected result

let test_insert () =
  let result = insert None "a" (`String "test value") in
  (check (option (of_pp Fmt.nop))) "Insert produces expected trie" (sample_leaf) result

let test_lookup () =
  let result = lookup sample_leaf "a" in
  let expected = Some sample_leaf in
  (check (option (of_pp Fmt.nop))) "Lookup expected result" expected (Some result)


let test_hash () =
  let result = hash sample_leaf in
  let expected = 
    "f0bde8c4cb492e1bdd686ed85d73065ec6a105e545d4498ab90207203488f979"
  in
  (check string) "Hash matches expected" expected result

let test_serialize () =
  let result = serialize (Leaf ([1;2;3], "test")) in
  let expected = "\206\132leaf\131123\132test" in
  (check string) "Serialize matches expected" expected result

let () =
  run "MKPTrie Tests" [
    "string_of_node", [test_case "string_of_node" `Quick test_string_of_node];
    "string_to_nibbles", [test_case "string_to_nibbles" `Quick test_string_to_nibbles];
    "nibbles_to_string", [test_case "nibbles_to_string" `Quick test_nibbles_to_string];
    "common_prefix_length", [test_case "common_prefix_length" `Quick test_common_prefix_length];
    "insert", [test_case "insert" `Quick test_insert];
    "lookup", [test_case "lookup" `Quick test_lookup];
    "hash node", [test_case "hash" `Quick test_hash];
    "serialize", [test_case "serialize" `Quick test_serialize];
  ]
