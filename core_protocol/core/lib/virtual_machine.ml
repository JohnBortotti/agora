open Ctypes
open Foreign

(* TODO: please doc the VM architecture *)

let target_path = Sys.getenv "VM_LIB_PATH" 
let rust_lib = Dl.dlopen ~filename:target_path ~flags:[Dl.RTLD_NOW]

let ffi_create_vm_server = foreign ~from:rust_lib "ffi_create_vm_server" (void @-> returning (ptr void))

let ffi_spawn_vm =
  foreign ~from:rust_lib  "ffi_spawn_vm"
    (ptr void @-> string @-> string @-> string @-> int32_t @-> int32_t @-> int32_t @-> int32_t @-> string @-> string @-> returning string)

let ffi_send_data_to_vm = 
  foreign ~from:rust_lib "ffi_send_data_to_vm" (ptr void @-> string @-> string @-> returning void)

let ffi_destroy_vm_server = foreign ~from:rust_lib "ffi_destroy_vm_server" (ptr void @-> returning void)

module VM = struct
  type t = {
    server: unit ptr;
  }

  let spawn_vm vm_server hash sender receiver amount gas_limit gas_price nonce payload signature =
    if is_null vm_server.server then failwith "VM server is not initialized properly";
    let result_str = 
      ffi_spawn_vm vm_server.server 
      hash sender receiver amount gas_limit gas_price nonce payload signature in
    if result_str = "" then failwith "Failed to spawn VM";
    result_str

  let send_data vm_server vm_id json_str =
    if is_null vm_server.server then failwith "VM server is not initialized";
    ffi_send_data_to_vm vm_server.server vm_id json_str

  let request_data_callback f vm_server vm_id req_json =
    let open Lwt.Syntax in

    let* result = (f req_json) in
    let result_str = Yojson.Basic.to_string result in

    send_data vm_server vm_id result_str;

    Lwt.return_unit

  let create f =
    let server_ptr = ffi_create_vm_server () in
    if is_null server_ptr then failwith "Failed to create VM server";
    let vm_server = { server = server_ptr } in
    Callback.register "ocaml_json_rpc_callback" (request_data_callback f vm_server);
    vm_server
  
  let destroy vm_server =
    if not (is_null vm_server.server) then (
      ffi_destroy_vm_server vm_server.server;
      Printf.printf "VM server destroyed successfully.\n";
    )

  let execute_vm tx callback_fn =
    let open Transaction in
    let server = create callback_fn in
    let vm_id = spawn_vm
      server
      tx.hash
      tx.sender
      tx.receiver
      (Int32.of_int tx.amount)
      (Int32.of_int tx.gas_limit)
      (Int32.of_int tx.gas_price)
      (Int32.of_int tx.nonce)
      tx.payload
      tx.signature
    in
    Printf.printf "[Ocaml] VM %s finished execution\n" vm_id;
    destroy server;
    "mocked vm return"
end
