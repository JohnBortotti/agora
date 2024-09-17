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
  val apply_transaction: MKPTrie.trie -> Transaction.t -> (MKPTrie.trie, string) result
  val apply_transactions: MKPTrie.trie -> Transaction.t list -> MKPTrie.trie
  val apply_transaction_coinbase: MKPTrie.trie -> Transaction.t -> (MKPTrie.trie, string) result
  val apply_block_transactions: MKPTrie.trie -> Transaction.t list -> MKPTrie.trie
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
  val revert_to_hash: t -> string -> t
end
