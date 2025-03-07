type debug_event = {
  timestamp: float;
  module_name: string;
  event_type: string;
  details: Yojson.Safe.t;
}

val init_debug : bool -> string -> unit
val log_event : module_name:string -> string -> Yojson.Safe.t -> unit Lwt.t
val save_debug_data : unit -> unit Lwt.t
val debug_persistence_routine : unit -> unit Lwt.t