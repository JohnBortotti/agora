open Alcotest
open Agora_core.State.RLP

let sample_string = `String "hello"
let sample_list = `List [`String "hello"; `String "world"]

let test_int_to_bytes () =
  let result = int_to_bytes 255 in
  let expected = [255] in
  (check (list int)) "int_to_bytes produces correct byte list" expected result

let test_encode_string () =
  let result = encode sample_string in
  let expected = "\133hello" in
  (check string) "RLP encode of string matches expected" expected result

let test_encode_list () =
  let result = encode sample_list in
  let expected = "\204\133hello\133world" in
  (check string) "RLP encode of list matches expected" expected result

let test_decode_string () =
  let encoded = encode sample_string in
  let result = decode encoded in
  (check (of_pp Fmt.nop)) "RLP decode of string matches original" sample_string result

let test_decode_list () =
  let encoded = encode sample_list in
  let result = decode encoded in
  (check (of_pp Fmt.nop)) "RLP decode of list matches original" sample_list result

let () =
  run "RLP Tests" [
    "int_to_bytes", [test_case "int_to_bytes" `Quick test_int_to_bytes];
    "encode_string", [test_case "encode_string" `Quick test_encode_string];
    "encode_list", [test_case "encode_list" `Quick test_encode_list];
    "decode_string", [test_case "decode_string" `Quick test_decode_string];
    "decode_list", [test_case "decode_list" `Quick test_decode_list];
  ]
