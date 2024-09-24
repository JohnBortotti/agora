open Yojson.Basic.Util
open Lwt.Syntax

type id = [ `Null | `Int of int | `String of string ]

type jsonrpc_request = {
  jsonrpc: string;
  method_name: string;
  params: Yojson.Basic.t option;
  id: id;
}

type jsonrpc_response = {
  jsonrpc: string;
  result: Yojson.Basic.t option;
  error: Yojson.Basic.t option;
  id: id;
}

let parse_request json =
  let jsonrpc = json |> member "jsonrpc" |> to_string in
  let method_name = json |> member "method" |> to_string in
  let params = json |> member "params" |> to_option Fun.id in
  let id_json = json |> member "id" in
  let id =
    match id_json with
    | `Null -> `Null
    | `Int n -> `Int n
    | `String s -> `String s
    | _ -> failwith "Invalid 'id' field in request: must be null, integer, or string"
  in
  { jsonrpc; method_name; params; id }

let create_request ~meth ~params ~id =
  {
    jsonrpc = "2.0";
    method_name = meth;
    params = params;
    id = id;
  }

let request_to_yojson (req: jsonrpc_request) =
  `Assoc (
    [ ("jsonrpc", `String req.jsonrpc);
      ("method", `String req.method_name);
      ("id", match req.id with
        | `Null -> `Null
        | `Int n -> `Int n
        | `String s -> `String s)
    ] @
    (match req.params with
     | Some params -> [ ("params", params) ]
     | None -> [])
  )

let request_to_string req =
  req |> request_to_yojson |> Yojson.Basic.to_string

let parse_response_from_string s =
    try
      let json = Yojson.Basic.from_string s in
      match json with
      | `Assoc fields ->
        let jsonrpc = List.assoc "jsonrpc" fields |> Yojson.Basic.Util.to_string in
        let id = List.assoc "id" fields in
        let id =
          match id with
          | `Null -> `Null
          | `Int n -> `Int n
          | `String s -> `String s
          | _ -> failwith "Invalid 'id' field in response"
        in
        let result = List.assoc_opt "result" fields in
        let error = List.assoc_opt "error" fields in
        Ok { jsonrpc; result; error; id }
      | _ -> Error "Invalid response format"
    with
    | Yojson.Json_error msg -> Error ("JSON parsing error: " ^ msg)
    | Failure msg -> Error ("Parsing error: " ^ msg)
    | Not_found -> Error "Missing required fields in response"

let create_response ?error ?result id =
  `Assoc (
    [ ("jsonrpc", `String "2.0");
      ("id", match id with
        | `Null -> `Null
        | `Int n -> `Int n
        | `String s -> `String s)
    ] @
    (match result with
     | Some r -> [ ("result", r) ]
     | None -> []) @
    (match error with
     | Some e -> [ ("error", e) ]
     | None -> [])
  )

let create_error code message data =
  `Assoc (
    [ ("code", `Int code);
      ("message", `String message)
    ] @
    (match data with
     | Some d -> [ ("data", d) ]
     | None -> [])
  )

let validate_request (req: jsonrpc_request) =
  if req.jsonrpc <> "2.0" then
    Error (create_error (-32600) "Invalid JSON-RPC version" None)
  else if req.method_name = "" then
    Error (create_error (-32600) "Method not specified" None)
  else
    Ok req

let response_to_string response =
  Yojson.Basic.to_string response

let parse_request_from_string json_string =
  try
    let json = Yojson.Basic.from_string json_string in
    match parse_request json with
    | req -> Ok req
    | exception Failure msg -> Error (create_error (-32600) msg None)
  with
  | Yojson.Json_error msg -> Error (create_error (-32700) ("Parse error: " ^ msg) None)

let handle_single_request method_registry json =
  match parse_request json with
  | req -> (
      match validate_request req with
      | Ok validated_req -> (
          match method_registry validated_req.method_name with
          | Some method_handler ->
              let* result = method_handler validated_req.params in
              let response = create_response ~result:(result) validated_req.id in
              Lwt.return response
          | None ->
              let error = create_error (-32601) "Method not found" None in
              let response = create_response ~error:(error) validated_req.id in
              Lwt.return response
        )
      | Error err ->
          let response = create_response ~error:(err) req.id in
          Lwt.return response
    )
  | exception Failure msg ->
      let error = create_error (-32600) msg None in
      let response = create_response ~error:(error) `Null in
      Lwt.return response

let handle_batch_request method_registry json_list =
  Lwt_list.map_p (fun json ->
      match json with
      | `Assoc _ -> handle_single_request method_registry json
      | _ ->
          let error = create_error (-32600) "Invalid Request" None in
          let response = create_response ~error:(error) `Null in
          Lwt.return response
    ) json_list

let handle_request method_registry request_json =
  match Yojson.Basic.from_string request_json with
  | `List json_list ->
      let* responses = handle_batch_request method_registry json_list in
      Lwt.return (`List responses)
  | json ->
      handle_single_request method_registry json
  | exception Yojson.Json_error msg ->
      let error = create_error (-32700) ("Parse error: " ^ msg) None in
      let response = create_response ~error:(error) `Null in
      Lwt.return response

