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
    let hex_of_string str =
      String.concat "" (List.map (Printf.sprintf "%02x") (List.map Char.code (String.to_seq str |> List.of_seq)))
    in
    match node with
    | Some (Leaf (k, v)) ->
      let decoded_value = match RLP.decode v with
        | `String s -> s
        | `List l -> "[" ^ (String.concat "; " (List.map (function `String s -> s | _ -> "<complex>") l)) ^ "]"
        | _ -> "<complex>"
      in
      Printf.sprintf "Leaf -> (key: %s, value: %s)" (hex_of_string k) decoded_value
    | Some (Extension (prefix, child)) ->
      Printf.sprintf "Extension (key: %s):\n%s" (if String.length prefix > 0 then hex_of_string prefix else "<>") (string_of_node (Some child) (level+1))
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

  let nibbles_to_string (nibbles: int list) : string =
    let rec group_nibbles nibs acc =
      match nibs with
      | [] -> List.rev acc
      | n1 :: n2 :: rest ->
          let byte = (n1 lsl 4) lor n2 in
          group_nibbles rest (Char.chr byte :: acc)
      | [n1] ->
          let byte = (n1 lsl 4) lor 0 in
          group_nibbles [] (Char.chr byte :: acc)
    in
    let char_list = group_nibbles nibbles [] in
    String.concat "" (List.map (String.make 1) char_list)

  let compare_nibbles (n1: int list) (n2: int list) : bool =
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
          Some (Leaf (nibbles_to_string nibbles, RLP.encode value))
      | Some (Leaf (k, v)) ->
          let key_nibbles = string_to_nibbles k in
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
              children.(idx) <- Some (Leaf (nibbles_to_string (List.tl remaining_nibbles), RLP.encode value));
              let branch = Branch (children, Some (RLP.encode (RLP.decode v))) in
              Some (Extension (nibbles_to_string (common_prefix), branch))
            else if remaining_nibbles = [] then
              let children = Array.make 16 None in
              let idx = List.hd remaining_key in
              children.(idx) <- Some (Leaf (nibbles_to_string (List.tl remaining_key), RLP.encode (RLP.decode v)));
              let branch = Branch (children, Some (RLP.encode value)) in
              Some (Extension (nibbles_to_string (common_prefix), branch))
            else 
              let children = Array.make 16 None in
              let idx1 = List.hd remaining_key in
              let idx2 = List.hd remaining_nibbles in
              children.(idx1) <- Some (Leaf (nibbles_to_string (List.tl remaining_key), RLP.encode (RLP.decode v)));
              children.(idx2) <- Some (Leaf (nibbles_to_string (List.tl remaining_nibbles), RLP.encode value));
              let branch = Branch (children, None) in
              Some (Extension (nibbles_to_string common_prefix, branch))
      | Some (Extension (prefix, next_node)) ->
          let prefix_nibbles = string_to_nibbles prefix in
          let common_prefix_len = common_prefix_length prefix_nibbles nibbles in 
          
          if common_prefix_len = List.length prefix_nibbles then
            let remaining_nibbles = list_sub nibbles common_prefix_len (List.length nibbles - common_prefix_len) in
            (match insert_aux (Some next_node) remaining_nibbles value with
            | Some new_node -> Some (Extension (prefix, new_node))
            | None -> failwith "Unexpected None while inserting in Extension node")
          else 
            failwith "EXTENSION NO"
      | Some (Branch (children, existing_value)) -> 
          (match existing_value with
          | None -> Printf.printf "no existing_value"
          | Some v -> Printf.printf "existing_value %s\n" v);

          match nibbles with
          | [] -> Some (Branch (children, Some (RLP.encode value)))
          | hd :: tl -> 
              Printf.printf "hd nibble: %d\n" hd;
              Printf.printf "children len %d\n" (Array.length children);
              Printf.printf "children node: %s\n" (string_of_node children.(hd) 0);
              if hd >= 0 && hd < Array.length children then
                children.(hd) <- insert_aux children.(hd) tl value;
              Some (Branch (children, existing_value))

          (* failwith "BRANCH" *)
      (* | Some (Leaf (k, v)) -> *)
      (*     let key_nibbles = string_to_nibbles k in *)
      (*     if key_nibbles = nibbles then *)
      (*       Some (Leaf (k, RLP.encode value)) *)
      (*     else *)
      (*       let common_prefix_len = common_prefix_length key_nibbles nibbles in *)
      (*       let common_prefix = list_sub nibbles 0 common_prefix_len in *)
      (*       let remaining_key = list_sub key_nibbles common_prefix_len (List.length key_nibbles - common_prefix_len) in *)
      (*       let remaining_nibbles = list_sub nibbles common_prefix_len (List.length nibbles - common_prefix_len) in *)
      (*        *)
      (*       if remaining_key = [] then *)
      (*         Some (Branch (Array.make 16 None, Some (RLP.encode value))) *)
      (*       else if remaining_nibbles = [] then *)
      (*         Some (Branch (Array.make 16 None, Some (RLP.encode (RLP.decode v)))) *)
      (*       else *)
      (*         let branch = create_branch_node remaining_key remaining_nibbles (RLP.decode v) value in *)
      (*         Some (Extension (nibbles_to_string common_prefix, branch)) *)
      (* | Some (Extension (prefix, next_node)) -> *)
      (*     let prefix_nibbles = string_to_nibbles prefix in *)
      (*     let common_prefix_len = common_prefix_length prefix_nibbles nibbles in *)
      (*     if common_prefix_len = List.length prefix_nibbles then *)
      (*       match insert_aux  *)
      (*         (Some next_node) (list_sub nibbles common_prefix_len (List.length nibbles - common_prefix_len))  *)
      (*       value with *)
      (*       | Some new_node -> Some (Extension (prefix, new_node)) *)
      (*       | None -> failwith "Unexpected None while inserting in Extension node" *)
      (*     else *)
      (*       let new_branch = create_branch_node *)
      (*         (list_sub prefix_nibbles common_prefix_len (List.length prefix_nibbles - common_prefix_len)) *)
      (*         (list_sub nibbles common_prefix_len (List.length nibbles - common_prefix_len)) *)
      (*         (RLP.decode (RLP.encode value)) *)
      (*         value *)
      (*       in *)
      (*       Some (Extension (nibbles_to_string (list_sub prefix_nibbles 0 common_prefix_len), new_branch)) *)
      (* | Some (Branch (children, existing_value)) ->  *)
      (*     match nibbles with *)
      (*     | [] ->  *)
      (*         Some (Branch (children, Some (RLP.encode value))) *)
      (*     | hd :: tl -> *)
      (*         if hd >= 0 && hd < Array.length children then *)
      (*           children.(hd) <- insert_aux children.(hd) tl value; *)
      (*         Some (Branch (children, existing_value)) *)
    in
    insert_aux t (string_to_nibbles key) value

  let rec lookup trie key =
    let nibbles = string_to_nibbles key in
    lookup_with_nibbles trie nibbles

  and lookup_with_nibbles (t: trie) (nibbles: int list): node option =
    match t with
    | None -> None
    | Some (Leaf (stored_key, value)) ->
        let stored_nibbles = string_to_nibbles stored_key in
        if compare_nibbles stored_nibbles nibbles then
          Some (Leaf (stored_key, value))
      else
        None
    | Some (Extension (shared_prefix, next_node)) ->
      let prefix_nibbles = string_to_nibbles shared_prefix in
      let prefix_len = List.length prefix_nibbles in
      if compare_nibbles prefix_nibbles (take prefix_len nibbles) then
        lookup_with_nibbles (Some next_node) (drop prefix_len nibbles)
      else
        None
    | Some (Branch (children, value_opt)) ->
        match nibbles with
        | [] -> (match value_opt with
             | Some value -> Some (Leaf ("", value))
             | None -> None)
        | next_nibble :: remaining_nibbles ->
            let next_child = children.(next_nibble) in
            lookup_with_nibbles next_child remaining_nibbles
end
