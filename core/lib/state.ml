open Transaction
open Digestif.SHA256

module RLP = struct
  type t = [ `String of string | `List of t list ]

  let int_to_bytes n =
    let rec aux n acc =
      if n = 0 then acc
      else aux (n lsr 8) ((n land 0xff) :: acc)
    in
    match aux n [] with
    | [] -> [0]
    | bytes -> bytes

  let encode_string s = 
    let len = String.length s in
    if len = 1 && Char.code s.[0] < 0x80 then
      s
    else if len <= 55 then 
      String.concat "" [String.make 1 (Char.chr (0x80 + len)); s]
    else
      let len_bytes = int_to_bytes len in
      String.concat "" [
        String.make 1 (Char.chr (0xB7 + List.length len_bytes));
        String.init (List.length len_bytes) (fun i -> Char.chr (List.nth len_bytes i));
        s
      ]

  let encode_list lst =
    let concatenated = String.concat "" lst in
    let len = String.length concatenated in
    if len <= 55 then
      String.concat "" [String.make 1 (Char.chr (0xC0 + len)); concatenated]
    else 
      let len_bytes = int_to_bytes len in
      String.concat "" [
        String.make 1 (Char.chr (0xF7 + List.length len_bytes));
        String.init (List.length len_bytes) (fun i -> Char.chr (List.nth len_bytes i));
        concatenated
      ]

  let rec encode = function
    | `String s -> encode_string s
    | `List lst -> encode_list (List.map encode lst)

  let int_of_bytes s =
    let len = String.length s in
    let rec aux acc i =
      if i >= len then acc
      else
        let byte = Char.code s.[i] in
        aux (acc lsl 8 + byte) (i + 1)
    in
    aux 0 0

  let decode s =
    let decode_string pos len =
      String.sub s pos len
    in
    let rec decode_list pos len =
      let rec aux acc pos limit =
        if pos >= limit then List.rev acc
        else
          let item, next_pos = decode_at pos in
          aux (item :: acc) next_pos limit
      in
      aux [] pos (pos + len)
    and decode_at pos =
      try
        let byte = Char.code s.[pos] in
        if byte < 0x80 then
          (`String (String.make 1 s.[pos]), pos + 1)
        else if byte <= 0xB7 then
          let len = byte - 0x80 in
          (`String (decode_string (pos + 1) len), pos + 1 + len)
        else if byte <= 0xBF then
          let len_len = byte - 0xB7 in
          let len = int_of_bytes (String.sub s (pos + 1) len_len) in
          (`String (decode_string (pos + 1 + len_len) len), pos + 1 + len_len + len)
        else if byte <= 0xF7 then
          let len = byte - 0xC0 in
          (`List (decode_list (pos + 1) len), pos + 1 + len)
        else
          let len_len = byte - 0xF7 in
          let len = int_of_bytes (String.sub s (pos + 1) len_len) in
          (`List (decode_list (pos + 1 + len_len) len), pos + 1 + len_len + len)
      with
      | Invalid_argument msg ->
          Printf.printf "Error: Invalid argument at position %d: %s\n" pos msg;
          raise (Failure "RLP decoding failed due to invalid argument")
      | Failure msg ->
          Printf.printf "Error: Failure at position %d: %s\n" pos msg;
          raise (Failure "RLP decoding failed")
      | _ ->
          Printf.printf "Error: Unknown exception at position %d\n" pos;
          raise (Failure "RLP decoding failed due to unknown exception")
    in
    fst (decode_at 0)
end

module MKPTrie = struct
  type node =
    | Leaf of nibbles * string
    | Extension of nibbles * node
    | Branch of (node option array) * (string option)
  and nibbles = int list

  type trie = node option

  let rec string_of_node node level =
    let indent = String.make (level * 2) ' ' in
    match node with
    | Some (Leaf (k, v)) ->
      let decoded_value = match RLP.decode v with
        | `String s -> s
        | `List l -> "[" ^ (String.concat "; " (List.map (function `String s -> s | _ -> "<complex>") l)) ^ "]"
        | _ -> "<complex>"
      in
      Printf.sprintf "Leaf -> (key: %s, value: %s)" (nibbles_to_string k) decoded_value
    | Some (Extension (prefix, child)) ->
      Printf.sprintf "Extension (key: %s):\n%s" 
        (if List.length prefix > 0 then (nibbles_to_string prefix) else "<>") (string_of_node (Some child) (level+1))
    | Some (Branch (children, value)) -> 
      let decoded_value = match value with
      | Some v -> (match RLP.decode v with
          | `String s -> s
          | `List l -> "[" ^ (String.concat "; " (List.map (function `String s -> s | _ -> "<complex>") l)) ^ "]"
          | _ -> "<complex>")
      | None -> "<>"
      in
      let children_str = Array.mapi (fun i child -> 
        if Option.is_some child then
          Printf.sprintf "%s[%x] -> %s" indent i (string_of_node child (level + 1))
        else ""
      ) children
      |> Array.to_list
      |> List.filter (fun s -> s <> "")
      |> String.concat "\n"
      in
      Printf.sprintf "%sBranch (val: %s):\n%s" indent decoded_value children_str
    | None -> Printf.sprintf "%s<>" indent

  and string_to_nibbles key =
    let nibbles = ref [] in
    String.iter (fun c ->
      let byte = Char.code c in
      nibbles := !nibbles @ [byte lsr 4; byte land 0xf]
    ) key;
    !nibbles

  and nibbles_to_string (nibbles: int list) : string =
    let nibble_strings = List.map string_of_int nibbles in
    String.concat "" nibble_strings

  let compare_nibbles (n1: int list) (n2: int list) : bool =
    if List.length n1 <> List.length n2 then
      false
    else
      List.for_all2 (fun a b -> a = b) n1 n2

  let rec common_prefix_length l1 l2 =
    match (l1, l2) with
    | (x :: xs, y :: ys) when x = y -> 1 + common_prefix_length xs ys
    | _ -> 0

  let rec take (n : int) (lst : int list) : int list =
    match n, lst with
    | 0, _ | _, [] -> []
    | n, x :: xs -> x :: take (n - 1) xs

  let rec drop n lst =
    match n, lst with
    | 0, lst -> lst
    | _, [] -> []
    | n, _ :: xs -> drop (n - 1) xs
  
  let list_sub lst start len =
    let rec aux lst i len acc =
      match lst with
      | [] -> List.rev acc
      | _ when i < start -> aux (List.tl lst) (i + 1) len acc
      | _ when len = 0 -> List.rev acc
      | hd :: tl -> aux tl (i + 1) (len - 1) (hd :: acc)
  in
  aux lst 0 len []

  let insert (t: trie) (key: string) (value: RLP.t): trie =
    let rec insert_aux node nibbles value =
      match node with
      | None -> 
          Some (Leaf (nibbles, RLP.encode value))
      | Some (Leaf (k, v)) ->
          let key_nibbles = k in
          if key_nibbles = nibbles then 
            Some (Leaf (k, RLP.encode value))
          else
            let common_prefix_len = common_prefix_length key_nibbles nibbles in
            let common_prefix = list_sub nibbles 0 common_prefix_len in
            let remaining_key = list_sub key_nibbles common_prefix_len (List.length key_nibbles - common_prefix_len) in
            let remaining_nibbles = list_sub nibbles common_prefix_len (List.length nibbles - common_prefix_len) in

            if remaining_key = [] then
              let children = Array.make 16 None in
              let idx = List.hd remaining_nibbles in
              children.(idx) <- Some (Leaf ((List.tl remaining_nibbles), RLP.encode value));
              let branch = Branch (children, Some (RLP.encode (RLP.decode v))) in
              Some (Extension ((common_prefix), branch))
            else if remaining_nibbles = [] then
              let children = Array.make 16 None in
              let idx = List.hd remaining_key in
              children.(idx) <- Some (Leaf ((List.tl remaining_key), RLP.encode (RLP.decode v)));
              let branch = Branch (children, Some (RLP.encode value)) in
              Some (Extension ((common_prefix), branch))
            else 
              let children = Array.make 16 None in
              let idx1 = List.hd remaining_key in
              let idx2 = List.hd remaining_nibbles in
              children.(idx1) <- Some (Leaf ((List.tl remaining_key), RLP.encode (RLP.decode v)));
              children.(idx2) <- Some (Leaf ((List.tl remaining_nibbles), RLP.encode value));
              let branch = Branch (children, None) in
              Some (Extension (common_prefix, branch))
      | Some (Extension (prefix, next_node)) ->
          let prefix_nibbles = prefix in
          let common_prefix_len = common_prefix_length prefix_nibbles nibbles in 
          
          if common_prefix_len = List.length prefix_nibbles then
            let remaining_nibbles = list_sub nibbles common_prefix_len (List.length nibbles - common_prefix_len) in
            (match insert_aux (Some next_node) remaining_nibbles value with
            | Some new_node -> Some (Extension (prefix, new_node))
            | None -> failwith "Unexpected None while inserting in Extension node")
          else 
            let children = Array.make 16 None in
            let remaining_prefix = list_sub prefix_nibbles common_prefix_len (List.length prefix_nibbles - common_prefix_len) in
            let remaining_nibbles = list_sub nibbles common_prefix_len (List.length nibbles - common_prefix_len) in

            print_endline (nibbles_to_string remaining_prefix);
            print_endline (nibbles_to_string remaining_nibbles);

            let idx1 = List.hd remaining_prefix in
            let idx2 = List.hd remaining_nibbles in
            children.(idx1) <- Some (Extension (List.tl remaining_prefix, next_node));
            children.(idx2) <- Some (Leaf ((List.tl remaining_nibbles), RLP.encode value));
            Some (Extension ((list_sub prefix_nibbles 0 common_prefix_len), Branch (children, None)))
      | Some (Branch (children, existing_value)) -> 
          match nibbles with
          | [] -> Some (Branch (children, Some (RLP.encode value)))
          | hd :: tl -> 
              if hd >= 0 && hd < Array.length children then
                children.(hd) <- insert_aux children.(hd) tl value;
              Some (Branch (children, existing_value))
    in
    insert_aux t (string_to_nibbles key) value

  let rec lookup trie key =
    let nibbles = string_to_nibbles key in
    lookup_with_nibbles trie nibbles
  and lookup_with_nibbles (t: trie) (nibbles: int list): node option =
    match t with
    | None -> None
    | Some (Extension (shared_prefix, next_node)) ->
      let prefix_nibbles = shared_prefix in
      let prefix_len = List.length prefix_nibbles in
      if compare_nibbles prefix_nibbles (take prefix_len nibbles) then
          lookup_with_nibbles (Some next_node) (drop prefix_len nibbles)
      else
        None
    | Some (Branch (children, value_opt)) ->
      (match nibbles with
      | [] ->
          (match value_opt with
          | Some value -> Some (Leaf ([], value))
          | None -> None)
      | hd :: tl ->
          if hd >= 0 && hd < Array.length children then
            lookup_with_nibbles children.(hd) tl
          else
            None)
    | Some (Leaf (stored_key, value)) ->
      let stored_nibbles = stored_key in
      if compare_nibbles stored_nibbles nibbles then
        Some (Leaf (stored_key, value))
      else
        None

  let rec hash = function
    | Some (Leaf (key, value)) ->
        let rlp_encoded = RLP.encode (`List[`String (nibbles_to_string key); `String value]) in
        to_hex (digest_string rlp_encoded)
    | Some (Extension (extension, child)) ->
        let child_hash = hash (Some child) in
        let rlp_encoded = RLP.encode (`List[`String (nibbles_to_string extension); `String child_hash]) in
        to_hex (digest_string rlp_encoded)
    | Some (Branch (children, value_opt)) ->
        let child_hashes = Array.map (function 
          | Some child -> hash (Some child)
          | None -> ""
        ) children in
        let rlp_encoded = RLP.encode (`List[
          `List (Array.to_list (Array.map (fun h -> `String h) child_hashes));
          (match value_opt with Some v -> `String v | None -> `String "")
        ]) in
        to_hex (digest_string rlp_encoded)
    | None -> ""

  let serialize_node = function
  | Leaf (nibbles, value) -> 
      RLP.encode_list [RLP.encode_string (nibbles_to_string nibbles); RLP.encode_string value]
  | Extension (nibbles, child) -> 
      let child_hash = hash (Some child) in
      RLP.encode_list [RLP.encode_string (nibbles_to_string nibbles); RLP.encode_string child_hash]
  | Branch (children, value_opt) -> 
      let children_rlp = Array.map (function
          | None -> RLP.encode_string ""
          | Some child -> RLP.encode_string (hash (Some child))
        ) children
      in
      let value_rlp = match value_opt with
        | Some value -> RLP.encode_string value
        | None -> RLP.encode_string ""
      in
      RLP.encode_list (Array.to_list children_rlp @ [value_rlp])

  let rec hash_iter f = function
  | None -> ()
  | Some node -> (
      let node_hash = hash (Some node) in
      let serialized_node = serialize_node node in
      f node_hash serialized_node;
      match node with
      | Leaf _ -> ()
      | Extension (_, child) -> hash_iter f (Some child)
      | Branch (children, _) -> Array.iter (hash_iter f) children
    )

end

module Account = struct
  type t = {
    address: string;
    balance: int;
    nonce: int;
    storage_root: string;
    code_hash: string;
  }

  let string_of_account account =
    Printf.sprintf "Address: %s\nBalance: %d\nNonce: %d\nStorage Root: %s\nCode Hash: %s\n"
      account.address
      account.balance
      account.nonce
      account.storage_root
      account.code_hash

  let encode account =
   `List [
      `String account.address;
      `String (string_of_int account.balance);
      `String (string_of_int account.nonce);
      `String account.storage_root;
      `String account.code_hash;
    ]

  let decode = function
    | `List [`String address; `String balance; `String nonce; `String storage_root; `String code_hash] ->
        { address; balance = int_of_string balance; nonce = int_of_string nonce; storage_root; code_hash }
    | _ -> failwith "Invalid RLP encoding for account"

  let get_account state address =
    match MKPTrie.lookup state address with
      | Some (MKPTrie.Leaf (_, account_data)) -> Some (decode (RLP.decode account_data))
      | Some (MKPTrie.Branch (_, some_data)) ->
          (match some_data with
          | Some account_data -> Some (decode (RLP.decode account_data))
          | _ -> None)
      | _ -> None

  let apply_transaction state tx =
    match get_account state tx.sender with
    | None -> Error "Sender account not found"
    | Some sender_account -> 
      if sender_account.balance < tx.amount then
        Error "Insufficient balance"
      else if sender_account.nonce <> tx.nonce then
        Error "Invalid nonce"
      else
        match get_account state tx.receiver with
      | None ->
        let new_receiver_account = {
          address = tx.receiver;
          balance = tx.amount;
          nonce = 0;
          storage_root = "";
          code_hash = "";
        } in
        let updated_sender_account = { sender_account with
          balance = sender_account.balance - tx.amount;
          nonce = sender_account.nonce + 1;
        } in
        let new_state = 
          MKPTrie.insert state new_receiver_account.address (encode new_receiver_account) 
          |> fun f -> MKPTrie.insert f sender_account.address (encode updated_sender_account) in
        Ok (new_state)
      | Some receiver_account ->
        let updated_receiver_account = { 
          receiver_account with balance = receiver_account.balance + tx.amount 
        } in
        let updated_sender_account = { sender_account with
          balance = sender_account.balance - tx.amount;
          nonce = sender_account.nonce + 1;
        } in
        let new_state = 
          MKPTrie.insert state receiver_account.address (encode updated_receiver_account) 
          |> fun f -> MKPTrie.insert f sender_account.address (encode updated_sender_account) in
        Ok (new_state)

  let apply_transaction_coinbase state tx =
    match get_account state tx.receiver with
    | None ->
      let new_receiver_account = {
        address = tx.receiver;
        balance = tx.amount;
        nonce = 0;
        storage_root = "";
        code_hash = "";
      } in
      let new_state = MKPTrie.insert state new_receiver_account.address (encode new_receiver_account) in
      Ok(new_state)
    | Some receiver_account ->
      let updated_receiver_account = {
        receiver_account with balance = receiver_account.balance + tx.amount
      } in
      let new_state =
        MKPTrie.insert state receiver_account.address (encode updated_receiver_account) in
      Ok(new_state)

end

module State = struct
  type t = {
    trie: MKPTrie.trie;
    db: Database.t;
  }

  let init_state db_path mem_size =
    let db = Database.create db_path mem_size in
    let trie = None in
    {trie; db}

  let get state key =
    MKPTrie.lookup state.trie key

  let set state key value =
    let new_trie = MKPTrie.insert state.trie key value in
    { state with trie = new_trie }

  (* TODO: create a module for util generic funcs *)
  let hex_of_bytes bytes =
    Bytes.fold_left (fun acc byte -> acc ^ Printf.sprintf "%02x" (int_of_char byte)) "" bytes

  let flush_to_db state =
    MKPTrie.hash_iter (fun key value ->
      let key_bytes = Bytes.of_string key in
      let value_bytes = Bytes.of_string value in

      let key_hex = hex_of_bytes key_bytes in
      let value_hex = hex_of_bytes value_bytes in

      Database.database_write state.db key_hex value_hex
    ) state.trie;
end
