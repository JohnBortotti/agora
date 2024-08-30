let generate_timestamp () =
  let time = Unix.gettimeofday () in
  let tm = Unix.gmtime time in
  Printf.sprintf "[%04d-%02d-%02d %02d:%02d:%02d]"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec

let setup_log_file filename =
  let oc = open_out filename in
  Logs.set_reporter (Logs_fmt.reporter ~dst:(Format.formatter_of_out_channel oc) ());
  Logs.set_level (Some Logs.Info)

let log_message level message =
  let timestamp = generate_timestamp () in
  let formatted_message = Printf.sprintf "%s %s" timestamp message in
  match level with
  | Logs.Info -> Logs.info (fun m -> m "%s" formatted_message)
  | Logs.Error -> Logs.err (fun m -> m "%s" formatted_message)
  | Logs.Warning -> Logs.warn (fun m -> m "%s" formatted_message)
  | Logs.Debug -> Logs.debug (fun m -> m "%s" formatted_message)
  | _ -> Logs.app (fun m -> m "%s" formatted_message)

(* let log_filename = "log_file_test.log" in *)
(* setup_log_file log_filename; *)
(* log_message Logs.Info "This is an info message"; *)
(* log_message Logs.Error "This is an error message"; *)
(* log_message Logs.Warning "This is a warning message"; *)
(* log_message Logs.Debug "This is a debug message"; *)
