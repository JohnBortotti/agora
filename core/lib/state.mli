module RLP : sig
  type t = [ `String of string | `List of t list ]

  val int_to_bytes: int -> int list
  val encode_string: string -> string
  val encode_list: string list -> string
  val encode: t -> string
  val decode: string -> t
end

module MKPTrie : sig
  type node =
    | Leaf of nibbles * string
    | Extension of nibbles * node
    | Branch of (node option array) * (string option)
  and nibbles = int list
  
  type trie = node option

  val string_of_node: trie -> int -> string
  val string_to_nibbles: string -> int list
  val nibbles_to_string: int list -> string
  val common_prefix_length: string list -> string list -> int
  val list_sub: string list -> int -> int -> string list
  val insert: trie -> string -> RLP.t -> trie
  val lookup: trie -> string -> node option
end

module Account : sig
  type t = {
    address: string;
    balance: int;
    nonce: int;
    storage_root: string;
    code_hash: string;
  }

  val encode: t -> RLP.t
  val decode: RLP.t -> t
end
