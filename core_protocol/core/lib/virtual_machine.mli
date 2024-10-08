module VM : sig
  type t = {
    server: unit Ctypes.ptr;
  }

  val execute_vm: Transaction.t -> (string -> Yojson.Basic.t Lwt.t) -> string
end
