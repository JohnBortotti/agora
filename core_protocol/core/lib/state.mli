module RLP : sig
  type t = [ `String of string | `List of t list ]

  val int_to_bytes: int -> int list
  val encode: t -> string
  val decode: string -> t
end

module MKPTrie : sig
  type node =
    | Leaf of int list * string
    | Extension of int list * node
    | Branch of (node option array) * (string option)
  type trie = node option

  val string_of_node: trie -> int -> string
  val string_to_nibbles: string -> int list
  val nibbles_to_string: int list -> string
  val common_prefix_length: string list -> string list -> int
  val insert: trie -> string -> RLP.t -> trie
  val lookup: trie -> string -> node option
  val hash: trie -> string
  val serialize: node -> string
end

module State : sig
  type t = {
    db: Database.t;
    mutable root_hash: string;
  }

  val init_state: string -> Unsigned.Size_t.t -> t
  (* TODO: add param f: function to decode result*)
  val get: t -> string -> string option
  (* TODO: add param f: function to encode*)
  val set: t -> string -> RLP.t -> unit
  val revert_to_hash: t -> string -> unit
end
