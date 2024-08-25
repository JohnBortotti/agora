open Digestif.SHA256

let has_leading_zeroes hash_binary n =
  let rec check_zeroes count = function
    | 0::tail -> check_zeroes (count + 1) tail
    | _ -> count >= n
  in 
  check_zeroes 0 hash_binary

let hex_to_bin hex_str =
  let int_of_char c =
    match c with
    | '0'..'9' -> Char.code c - Char.code '0'
    | 'a'..'f' -> 10 + Char.code c - Char.code 'a'
    | 'A'..'F' -> 10 + Char.code c - Char.code 'A'
    | _ -> failwith "Invalid hex character"
  in
  let rec hex_to_bits = function
    | [] -> []
    | c::cs ->
      let bits = [int_of_char c lsr 3; (int_of_char c lsr 2) land 1;
        (int_of_char c lsr 1) land 1; int_of_char c land 1] in
      bits @ (hex_to_bits cs)
  in
  hex_to_bits (List.of_seq (String.to_seq hex_str))

let proof_of_work message difficulty =
  let rec find_nonce nonce =
    let full_message = message ^ string_of_int nonce in
    let hash = to_hex (digest_string full_message) in
    if has_leading_zeroes (hex_to_bin hash) difficulty then nonce
    else find_nonce (nonce + 1)
  in 
  find_nonce 0
