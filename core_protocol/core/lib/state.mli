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
    type t = node option

  val string_of_node: t -> int -> string
  val string_to_nibbles: string -> int list
  val nibbles_to_string: int list -> string
  val common_prefix_length: string list -> string list -> int
  val insert: t -> string -> RLP.t -> t
  val lookup: t -> string -> node option
  val hash: t -> string
  val serialize: node -> string
end

module State : sig
  type t = {
    trie: MKPTrie.t;
    db: Database.t;
  }

  val init_state: string -> Unsigned.Size_t.t -> t
  val trie_get: t -> string -> MKPTrie.t
  val trie_set: t -> string -> RLP.t -> t
  val flush_trie_to_db: t -> unit
  val get_from_db: t -> string -> RLP.t option
  val revert_to_hash: t -> string -> t
end
