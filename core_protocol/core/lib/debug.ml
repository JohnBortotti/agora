type debug_event = {
  timestamp: float;
  module_name: string;
  event_type: string;
  details: Yojson.Safe.t;
}

let debug_enabled = ref false
let debug_file = ref "/home/opam/debug/debug_info.json"
let debug_events = ref []
let debug_mutex = Lwt_mutex.create ()

let init_debug enabled file_path =
  debug_enabled := enabled;
  debug_file := file_path;
  debug_events := []

let log_event ~module_name event_type details =
  if !debug_enabled then
    let open Lwt.Syntax in
    let event = {
      timestamp = Unix.time ();
      module_name;
      event_type;
      details;
    } in
    let* () = Lwt_mutex.lock debug_mutex in
    debug_events := event :: !debug_events;
    Lwt_mutex.unlock debug_mutex;
    Lwt.return_unit
  else
    Lwt.return_unit

let event_to_json event =
  `Assoc [
    ("timestamp", `Float event.timestamp);
    ("module_name", `String event.module_name);
    ("event_type", `String event.event_type);
    ("details", event.details)
  ]

let save_debug_data () =
  if !debug_enabled then
    let open Lwt.Syntax in
    let* () = Lwt_mutex.lock debug_mutex in
    let events = !debug_events in
    Lwt_mutex.unlock debug_mutex;
    
    let json_data = 
      `Assoc [
        ("debug_enabled", `Bool true);
        ("events", `List (List.map event_to_json events))
      ]
    in
    
    let json_string = Yojson.Safe.to_string json_data in
    Lwt_io.with_file ~mode:Lwt_io.output !debug_file (fun oc ->
      Lwt_io.write oc json_string
    )
  else
    Lwt.return_unit

let debug_persistence_routine () =
  let open Lwt.Syntax in
  let rec loop () =
    let* () = Lwt_unix.sleep 2.0 in
    let* () = save_debug_data () in
    loop ()
  in
  loop () 