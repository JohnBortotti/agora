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
      let byte = Char.code s.[pos] in
      if byte < 0x80 then
        (`String (String.make 1 s.[pos]), pos + 1)
      else if byte <= 0xB7 then
        let len = byte - 0x80 in
        (`String (decode_string (pos + 1) len), pos + 1 + len)
      else if byte <= 0xBF then
        let len_len = byte - 0xB7 in
        let len = int_of_string (String.sub s (pos + 1) len_len) in
        (`String (decode_string (pos + 1 + len_len) len), pos + 1 + len_len + len)
      else if byte <= 0xF7 then
        let len = byte - 0xC0 in
        (`List (decode_list (pos + 1) len), pos + 1 + len)
      else
        let len_len = byte - 0xF7 in
        let len = int_of_string (String.sub s (pos + 1) len_len) in
        (`List (decode_list (pos + 1 + len_len) len), pos + 1 + len_len + len)
    in
    fst (decode_at 0)
end

module MKPTrie = struct
  type node =
    | Leaf of string * string
    | Extension of string * node
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
        Printf.sprintf "Leaf -> (key: %s, value: %s)" k decoded_value
      | Some (Extension (prefix, child)) ->
        Printf.sprintf "Extension (key: %s):\n%s" (if String.length prefix > 0 then prefix else "<>") (string_of_node (Some child) (level+1))
      | Some (Branch (children, value)) -> 
        let children_str = Array.mapi (fun i child -> 
          if Option.is_some child then
            Printf.sprintf "%s[%x] -> %s" indent i (string_of_node child (level + 1))
          else ""
        ) children
        |> Array.to_list
        |> List.filter (fun s -> s <> "")
        |> String.concat "\n"
        in
        Printf.sprintf "%sBranch (val: %s):\n%s" indent (match value with Some v -> v | None -> "<>") children_str
      | None -> Printf.sprintf "%s<>" indent

  let string_to_nibbles key =
    let nibbles = ref [] in
    String.iter (fun c ->
      let byte = Char.code c in
      nibbles := !nibbles @ [byte lsr 4; byte land 0xf]
    ) key;
    !nibbles

  let string_of_nibbles nibbles =
    String.concat "" (List.map (Printf.sprintf "%x") nibbles)

  let rec common_prefix_length l1 l2 =
    match (l1, l2) with
    | (x :: xs, y :: ys) when x = y -> 1 + common_prefix_length xs ys
    | _ -> 0
  
  let list_sub lst start len =
    let rec aux lst i len acc =
      match lst with
      | [] -> List.rev acc
      | _ when i < start -> aux (List.tl lst) (i + 1) len acc
      | _ when len = 0 -> List.rev acc
      | hd :: tl -> aux tl (i + 1) (len - 1) (hd :: acc)
  in
  aux lst 0 len []

  let create_branch_node nibbles1 nibbles2 val1 val2 =
    let children = Array.make 16 None in
    children.(List.hd nibbles1) <- Some (Leaf (string_of_nibbles (List.tl nibbles1), RLP.encode val1));
    children.(List.hd nibbles2) <- Some (Leaf (string_of_nibbles (List.tl nibbles2), RLP.encode val2));
    Branch (children, None)

  let insert (t: trie) (key) (value): trie =
    let rec insert_aux node nibbles value =
      match node with
        | None -> 
            Some (Leaf (String.concat "" (List.map (Printf.sprintf "%x") nibbles), RLP.encode (value)))
        | Some (Leaf (k, v)) ->
            let key_nibbles = string_to_nibbles k in
            if key_nibbles = nibbles then
              Some (Leaf (k, RLP.encode value))
            else
              let common_prefix_len = common_prefix_length key_nibbles nibbles in
              let common_prefix = list_sub nibbles 0 common_prefix_len in
              let remaining_key = list_sub key_nibbles common_prefix_len (List.length key_nibbles - common_prefix_len) in
              let remaining_nibbles = list_sub nibbles common_prefix_len (List.length nibbles - common_prefix_len) in
              let branch = create_branch_node remaining_key remaining_nibbles (RLP.decode v) value in
              Some (Extension (string_of_nibbles common_prefix, branch))
        | Some (Extension (prefix, next_node)) ->
            let prefix_nibbles = string_to_nibbles prefix in
            let common_prefix_len = common_prefix_length prefix_nibbles nibbles in
            if common_prefix_len = List.length prefix_nibbles then
              match insert_aux (Some next_node) (list_sub nibbles common_prefix_len (List.length nibbles - common_prefix_len)) value with
              | Some new_node -> Some (Extension (prefix, new_node))
              | None -> failwith "Unexpected None while inserting in Extension node"
            else
              let new_branch = create_branch_node
                (list_sub prefix_nibbles common_prefix_len (List.length prefix_nibbles - common_prefix_len))
                (list_sub nibbles common_prefix_len (List.length nibbles - common_prefix_len))
                (RLP.decode (RLP.encode value))
                value
              in
              Some (Extension (string_of_nibbles (list_sub prefix_nibbles 0 common_prefix_len), new_branch))
        | Some (Branch (children, existing_value)) -> 
            match nibbles with
            | [] -> Some (Branch (children, Some (RLP.encode value)))
            | hd :: tl ->
                let idx = hd in
                children.(idx) <- insert_aux children.(idx) tl value;
                Some (Branch (children, existing_value))
    in
    insert_aux t (string_to_nibbles key) value

end
