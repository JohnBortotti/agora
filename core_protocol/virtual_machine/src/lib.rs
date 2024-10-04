mod vm;
mod u256;

use vm::server::VMServer;
use vm::vm_core::Transaction;
use std::ffi::{CStr, CString, c_char};
use uuid::Uuid;

#[no_mangle]
pub extern "C" fn ffi_create_vm_server() -> *mut VMServer {
  let server = VMServer::new();
  Box::into_raw(Box::new(server))
}

#[no_mangle]
pub extern "C" fn ffi_spawn_vm(
  server_ptr: *mut VMServer,
  hash: *const c_char,
  sender: *const c_char,
  receiver: *const c_char,
  amount: i32,
  gas_limit: i32,
  gas_price: i32,
  nonce: i32,
  payload: *const c_char,
  signature: *const c_char,
  ) -> *mut c_char {
  let server = unsafe {
    assert!(!server_ptr.is_null());
    &*server_ptr
  };

  let hash = unsafe { CStr::from_ptr(hash).to_string_lossy().into_owned() };
  let sender = unsafe { CStr::from_ptr(sender).to_string_lossy().into_owned() };
  let receiver = unsafe { CStr::from_ptr(receiver).to_string_lossy().into_owned() };
  let payload = unsafe { CStr::from_ptr(payload).to_string_lossy().into_owned() };
  let signature = unsafe { CStr::from_ptr(signature).to_string_lossy().into_owned() };

  let transaction = Transaction {
    hash,
    sender,
    receiver,
    amount,
    gas_limit,
    gas_price,
    nonce,
    payload,
    signature,
  };

  let vm_id = server.spawn_vm(transaction);
  let vm_id_string = CString::new(vm_id.to_string()).unwrap();
  vm_id_string.into_raw()
}

#[no_mangle]
pub extern "C" fn ffi_send_data_to_vm(
  server_ptr: *mut VMServer,
  vm_id: *const c_char,
  key: *const c_char,
  data: *const c_char) {
  let server = unsafe {
    assert!(!server_ptr.is_null());
    &*server_ptr
  };

  let vm_id_str = unsafe { CStr::from_ptr(vm_id).to_str().unwrap() };
  let key_str = unsafe { CStr::from_ptr(key).to_str().unwrap() };
  let data_str = unsafe { CStr::from_ptr(data).to_str().unwrap() };

  let vm_id = Uuid::parse_str(vm_id_str).unwrap();

  server.send_data_to_vm(vm_id, key_str.to_string(), data_str.to_string());
}
