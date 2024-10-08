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

module Account : sig
  type t = {
    address: string;
    balance: int;
    nonce: int;
    storage_root: string;
    code_hash: string;
  }

  val string_of_account: t -> string
  val account_to_json: t -> Yojson.Basic.t
  val encode: t -> RLP.t
  val decode: RLP.t -> t

  val apply_block_transactions: string -> MKPTrie.trie -> MKPTrie.trie -> Transaction.t list -> (Transaction.t -> string) -> (MKPTrie.trie * MKPTrie.trie)
end

module State : sig
  type t = {
    trie: MKPTrie.trie;
    db: Database.t;
  }

  val init_state: string -> Unsigned.Size_t.t -> t
  val trie_get: t -> string -> MKPTrie.trie
  val trie_set: t -> string -> RLP.t -> t
  val flush_to_db: t -> unit
  val get_from_db: t -> string -> RLP.t option
  val revert_to_hash: t -> string -> t
end
