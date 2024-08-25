(*
TODO:
  - [x] write and sign transaction
  - [ ] node
  - [ ] consensus (forks and conflicts)
  - [ ] vm and contracts
 *)

open Agora_core.State
(* open Cryptokit *)

let () = 
  (* let key = RSA.new_key 2048 in *)
  (* let test_tx: transaction = { *)
  (*   sender = "Alice"; *)
  (*   receiver = "Bob"; *)
  (*   amount = 100; *)
  (*   gas_limit = 500; *)
  (*   gas_price = 1; *)
  (*   nonce = 1; *)
  (*   payload = "Transfer 100 tokens from Alice to Bob"; *)
  (*   signature = ""; *)
  (* } in *)
  (* let signed_tx = { test_tx with signature = sign_transaction test_tx key } in *)
  (* print_endline""; *)
  (* print_endline (string_of_transaction signed_tx) *)

  (* let encoded = RLP.encode (`List [`String "alice"; `String "bob"]) in *)
  (* let hex_of_char c = Printf.sprintf "%02x" (Char.code c) in *)
  (* let encoded_hex = String.concat "" (List.map hex_of_char (String.to_seq encoded |> List.of_seq)) in *)
  (* Printf.printf "Encoded result: %s\n" encoded_hex; *)
  (* let decoded = RLP.decode encoded in *)
  (* let rec print_decoded = function *)
  (* | `String s -> Printf.printf "Decoded string: %s\n" s *)
  (* | `List lst -> *)
  (*     Printf.printf "Decoded list:\n"; *)
  (*     List.iter print_decoded lst *)
  (* in *)
  (* print_decoded decoded *)

  let trie = 
    MKPTrie.insert None "bob" (`List [`String "ETH"; `String "14.208"]) 
    |> fun x -> MKPTrie.insert x "alice" (`List [`String "ETH"; `String "07.104"])
    |> fun x -> MKPTrie.insert x "alex" (`List [`String "ETH"; `String "32.000"])
  in
  print_endline (MKPTrie.string_of_node trie 0);
