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
    | Leaf of int list * string
    | Extension of int list * node
    | Branch of (node option array) * (string option)

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

  let rec take n lst =
    match n, lst with
    | 0, _ | _, [] -> []
    | n, x :: xs -> x :: take (n - 1) xs

  let rec drop n lst =
    match n, lst with
    | 0, lst -> lst
    | _, [] -> []
    | n, _ :: xs -> drop (n - 1) xs
  
  let insert (t: trie) (key: string) (value: RLP.t): trie =
    let list_sub lst start len =
      let rec aux lst i len acc =
        match lst with
        | [] -> List.rev acc
        | _ when i < start -> aux (List.tl lst) (i + 1) len acc
        | _ when len = 0 -> List.rev acc
        | hd :: tl -> aux tl (i + 1) (len - 1) (hd :: acc)
    in
    aux lst 0 len []
    in
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

  let serialize = function
    | Leaf (nibbles, value) ->
        RLP.encode_list [
          RLP.encode_string "leaf";
          RLP.encode_string (nibbles_to_string nibbles);
          RLP.encode_string value
        ]
    | Extension (nibbles, child) ->
        let child_hash = hash (Some child) in
        RLP.encode_list [
          RLP.encode_string "extension";
          RLP.encode_string (nibbles_to_string nibbles);
          RLP.encode_string child_hash
        ]
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
        RLP.encode_list (
          RLP.encode_string "branch" ::
          (Array.to_list children_rlp @ [value_rlp])
        )
end

module State = struct
  type t = {
    db: Database.t;
    mutable root_hash: string;
  }

  let init_state db_path mem_size =
    let db = Database.create db_path mem_size in
    {db;root_hash=""}

  let hex_of_bytes bytes =
    Bytes.fold_left (fun acc byte -> acc ^ Printf.sprintf "%02x" (int_of_char byte)) "" bytes

  let bytes_of_hex_safe hex_str =
    let len = String.length hex_str in
    if len mod 2 <> 0 then failwith "Invalid hex string length";
    let bytes = Bytes.create (len / 2) in
    for i = 0 to (len / 2) - 1 do
      let hex_pair = String.sub hex_str (i * 2) 2 in
      try
        let byte_value = int_of_string ("0x" ^ hex_pair) in
        Bytes.set bytes i (char_of_int byte_value)
      with
      | Failure _ -> failwith ("Invalid hex value: " ^ hex_pair)
    done;
    Bytes.to_string bytes

  let nibbles_from_db_str s =
    let nibbles = ref [] in
      String.iter (fun c ->
        let nibble = int_of_string ("0x" ^ String.make 1 c) in
        nibbles := !nibbles @ [nibble]
      ) s;
      !nibbles

  let get_from_db state key =
    let key_bytes = Bytes.of_string key in
    let key_hex = hex_of_bytes key_bytes in
    match Database.get state.db key_hex with
    | None ->
        Printf.printf "Key not found in the database: %s\n" key;
        None
    | Some value_hex ->
        (try
           let value_str = bytes_of_hex_safe value_hex in
           let rlp_value = RLP.decode value_str in
           Some rlp_value
         with
         | Failure msg -> 
             Printf.printf "Error during conversion or RLP decoding: %s\n" msg; 
             None)

  let rec parse_node state serialized_str =
    let open MKPTrie in
    let rlp_node = RLP.decode serialized_str in
    match rlp_node with
    | `List (`String "leaf" :: `String nibbles_str :: `String value_str :: []) ->
        let nibbles = nibbles_from_db_str nibbles_str in
        Leaf (nibbles, value_str)
    | `List (`String "extension" :: `String nibbles_str :: `String child_hash :: []) ->
        let nibbles = nibbles_from_db_str nibbles_str in
        (match Database.get state.db child_hash with
        | None -> failwith ("Child node not found in database: " ^ child_hash)
        | Some child_serialized_str ->
            let child_node = parse_node state child_serialized_str in
            Extension (nibbles, child_node))
    | `List (`String "branch" :: child_rlp_list) when List.length child_rlp_list = 17 ->
        let children = Array.init 16 (fun i ->
          match List.nth child_rlp_list i with
          | `String "" -> None
          | `String child_hash ->
              (match Database.get state.db child_hash with
              | None -> None
              | Some child_serialized_str ->
                  Some (parse_node state child_serialized_str))
          | _ -> failwith "Invalid RLP structure for branch node"
        ) in
        let value_opt = match List.nth child_rlp_list 16 with
          | `String "" -> None
          | `String value_str -> Some value_str
          | _ -> failwith "Invalid RLP structure for branch node"
        in
        Branch (children, value_opt)
    | _ -> failwith "Invalid RLP structure"

  let rec match_nibbles key_nibbles node_nibbles =
    match (key_nibbles, node_nibbles) with
    | (k::ks, n::ns) when k = n ->
        let (common, rest_key, rest_node) = match_nibbles ks ns in
        (k::common, rest_key, rest_node)
    | _ -> ([], key_nibbles, node_nibbles)

  let rec trie_get state node key_nibbles =
    let open MKPTrie in
    match node with
    | Leaf (node_nibbles, value) ->
        if node_nibbles = key_nibbles then Some value else None
    | Extension (node_nibbles, child_node) ->
        let (_, rest_key, rest_node) = match_nibbles key_nibbles node_nibbles in
        if rest_node = [] then
          trie_get state child_node rest_key
        else
          None
    | Branch (children, value_opt) ->
        (match key_nibbles with
        | [] -> value_opt
        | idx::rest_nibbles ->
            (match children.(idx) with
            | None -> None
            | Some child_node -> trie_get state child_node rest_nibbles))

  let hash_string s =
    let open Digestif.SHA256 in
    to_hex (digest_string s)

  let write_node_to_db state node =
    let serialized = MKPTrie.serialize node in
    let node_hash = hash_string (serialized) in
    let node_hex = hex_of_bytes (Bytes.of_string (serialized)) in
    Database.write state.db node_hash node_hex;
    node_hash

  let rec trie_set state node_opt key_nibbles value =
    let open MKPTrie in
    match node_opt with
    | None ->
        (* The node is empty; create a new leaf node *)
        let leaf_node = Leaf (key_nibbles, value) in
        let _ = write_node_to_db state leaf_node in
        leaf_node
    | Some node ->
        match node with
        | Leaf (node_nibbles, node_value) ->
            (* Handle the case where we're at a leaf node *)
            let (common_prefix, rest_key, rest_node) = match_nibbles key_nibbles node_nibbles in
            if rest_node = [] && rest_key = [] then
              (* The key matches the existing leaf; update the value *)
              let leaf_node = Leaf (node_nibbles, value) in
              let _ = write_node_to_db state leaf_node in
              leaf_node
            else
              (* Need to create a branch node to resolve the conflict *)
              let new_leaf_node = Leaf (rest_key, value) in
              let _ = write_node_to_db state new_leaf_node in
              let existing_leaf_node = Leaf (rest_node, node_value) in
              let _ = write_node_to_db state existing_leaf_node in
              let branch_children = Array.make 16 None in
              (* Update the branch's children *)
              (match rest_key with
              | idx::rest when rest = [] ->
                  branch_children.(idx) <- Some new_leaf_node
              | idx::rest ->
                  let ext_node = Extension (rest, new_leaf_node) in
                  let _ = write_node_to_db state ext_node in
                  branch_children.(idx) <- Some ext_node
              | [] -> ());
              (match rest_node with
              | idx::rest when rest = [] ->
                  branch_children.(idx) <- Some existing_leaf_node
              | idx::rest ->
                  let ext_node = Extension (rest, existing_leaf_node) in
                  let _ = write_node_to_db state ext_node in
                  branch_children.(idx) <- Some ext_node
              | [] -> ());
              let branch_node = Branch (branch_children, None) in
              let _ = write_node_to_db state branch_node in
              if common_prefix = [] then
                branch_node
              else
                (* Create an extension node for the common prefix *)
                let extension_node = Extension (common_prefix, branch_node) in
                let _ = write_node_to_db state extension_node in
                extension_node
        | Extension (node_nibbles, child_node) ->
            (* Handle the case where we're at an extension node *)
            let (common_prefix, rest_key, rest_node) = match_nibbles key_nibbles node_nibbles in
            if rest_node = [] then
              (* The key matches the extension's nibbles; proceed to child *)
              let new_child_node = trie_set state (Some child_node) rest_key value in
              let extension_node = Extension (node_nibbles, new_child_node) in
              let _ = write_node_to_db state extension_node in
              extension_node
            else
              (* Split the extension node *)
              let new_leaf_node = Leaf (rest_key, value) in
              let _ = write_node_to_db state new_leaf_node in
              let existing_extension_node = Extension (rest_node, child_node) in
              let _ = write_node_to_db state existing_extension_node in
              let branch_children = Array.make 16 None in
              (match rest_key with
              | idx::rest when rest = [] ->
                  branch_children.(idx) <- Some new_leaf_node
              | idx::rest ->
                  let ext_node = Extension (rest, new_leaf_node) in
                  let _ = write_node_to_db state ext_node in
                  branch_children.(idx) <- Some ext_node
              | [] -> ());
              (match rest_node with
              | idx::rest when rest = [] ->
                  branch_children.(idx) <- Some existing_extension_node
              | idx::rest ->
                  let ext_node = Extension (rest, existing_extension_node) in
                  let _ = write_node_to_db state ext_node in
                  branch_children.(idx) <- Some ext_node
              | [] -> ());
              let branch_node = Branch (branch_children, None) in
              let _ = write_node_to_db state branch_node in
              if common_prefix = [] then
                branch_node
              else
                let extension_node = Extension (common_prefix, branch_node) in
                let _ = write_node_to_db state extension_node in
                extension_node
        | Branch (children, value_opt) ->
            (* Handle the case where we're at a branch node *)
            (match key_nibbles with
            | [] ->
                (* We're at the node corresponding to the key; update the value *)
                let branch_node = Branch (children, Some value) in
                let _ = write_node_to_db state branch_node in
                branch_node
            | idx::rest_nibbles ->
                (* Proceed to the appropriate child *)
                let child_node_opt = children.(idx) in
                let new_child_node = trie_set state child_node_opt rest_nibbles value in
                let new_children = Array.copy children in
                new_children.(idx) <- Some new_child_node;
                let branch_node = Branch (new_children, value_opt) in
                let _ = write_node_to_db state branch_node in
                branch_node)
          
  let get state key =
    let key_nibbles = MKPTrie.string_to_nibbles key in
    let curr_root_hash = state.root_hash in
    match curr_root_hash with
    | "" | "0" -> None
    | root_hash ->
        match get_from_db state root_hash with
        | None -> None
        | Some serialized_root ->
            let root_node = parse_node state (RLP.encode serialized_root) in
            (trie_get state root_node key_nibbles)

  let set state key value =
    let value_rlp = RLP.encode value in
    let key_nibbles = MKPTrie.string_to_nibbles key in
    let root_node_opt =
      if state.root_hash = "" || state.root_hash = "0" then
        None
      else
        match get_from_db state state.root_hash with
        | None -> failwith ("Root node not found in database: " ^ state.root_hash)
        | Some serialized_str -> Some (parse_node state (RLP.encode serialized_str))
    in
    let new_root_node = trie_set state root_node_opt key_nibbles value_rlp in
    let new_root_hash = write_node_to_db state new_root_node in
    Database.write state.db (to_hex (digest_string "root_hash")) (new_root_hash);
    ()
    
  let revert_to_hash state hash =
    let result = match Database.get state.db hash with
      | Some _ -> { state with root_hash = hash }
      | None -> failwith ("Hash not found in database: " ^ hash) 
  in 
  ()
end
