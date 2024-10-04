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
  foreign ~from:rust_lib "ffi_send_data_to_vm" (ptr void @-> string @-> string @-> string @-> returning void)

module VM = struct
  type t = {
    server: unit ptr;
  }

  let spawn_vm vm_server sender receiver amount gas_limit gas_price nonce payload signature =
    ffi_spawn_vm vm_server.server sender receiver amount gas_limit gas_price nonce payload signature

  let send_data vm_server vm_id key value =
    ffi_send_data_to_vm vm_server.server vm_id key value

  let request_data_callback vm_server vm_id method_name params =
    Printf.printf "Received JSON-RPC request: vm_id=%s, method=%s, params=%s\n" vm_id method_name params;
    let result =
      match method_name with
      | "get_account" -> "{\"balance\": 1000, \"nonce\": 1}"
      | _ -> "{\"error\": \"unknown method\"}"
    in
    send_data vm_server vm_id method_name result

  let create () =
    let server_ptr = ffi_create_vm_server () in
    Callback.register "ocaml_json_rpc_callback" (request_data_callback { server = server_ptr });
    { server = server_ptr }
end
