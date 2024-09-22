open Alcotest
open Digestif.SHA256
open Agora_core.Pow

let test_hex_to_bin () =
  check (list int) "hex_to_bin '0'" [0; 0; 0; 0] (hex_to_bin "0");
  check (list int) "hex_to_bin 'F'" [1; 1; 1; 1] (hex_to_bin "F");
  check (list int) "hex_to_bin '1A'"
    [0; 0; 0; 1; 1; 0; 1; 0] (hex_to_bin "1A");
  check (list int) "hex_to_bin '3C'"
    [0; 0; 1; 1; 1; 1; 0; 0] (hex_to_bin "3C");
  check (list int) "hex_to_bin '7F'"
    [0; 1; 1; 1; 1; 1; 1; 1] (hex_to_bin "7F")

let test_has_leading_zeroes () =
  check bool "Exactly 3 leading zeros"
    true
    (has_leading_zeroes [0;0;0;2] 3);

  check bool "No leading zeros in 'F000'"
    false
    (has_leading_zeroes (hex_to_bin "F000") 1);

  check bool "Empty list"
    false
    (has_leading_zeroes [] 1);

  check bool "Exactly 8 leading zeros in '00FF'"
    true
    (has_leading_zeroes (hex_to_bin "00FF") 8);

  check bool "No 9 leading zeros in '00FF'"
    false
    (has_leading_zeroes (hex_to_bin "00FF") 9)

let test_proof_of_work () =
  let nonce = proof_of_work "test" 1 in
  check bool "Nonce should produce a hash with at least 1 leading zero"
    true
    (has_leading_zeroes (hex_to_bin (to_hex (digest_string ("test" ^ string_of_int nonce)))) 1);

  let nonce = proof_of_work "difficult_test" 3 in
  check bool "Nonce should produce a hash with at least 3 leading zeros"
    true
    (has_leading_zeroes (hex_to_bin (to_hex (digest_string ("difficult_test" ^ string_of_int nonce)))) 3);

  let nonce = proof_of_work "difficult_test" 3 in
  check bool "Nonce should produce a hash without 4 leading zeros"
    false
    (has_leading_zeroes (hex_to_bin (to_hex (digest_string ("difficult_test" ^ string_of_int nonce)))) 4)

let () =
  run "Pow module tests" [
    "Hex_to_bin", [
      test_case "Test hex_to_bin function" `Quick test_hex_to_bin;
    ];
    "Leading_zeroes", [
      test_case "Test has_leading_zeroes function" `Quick test_has_leading_zeroes;
    ];
    "Proof_of_work", [
      test_case "Test proof_of_work function" `Quick test_proof_of_work
    ]
  ]

