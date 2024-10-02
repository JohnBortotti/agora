mod vm;
mod u256;

use vm::server::VMServer;
use vm::vm_core::Transaction;
use std::ffi::{CStr, CString, c_char};

#[no_mangle]
pub extern "C" fn create_vm_server() -> *mut VMServer {
  let server = VMServer::new();
  Box::into_raw(Box::new(server))
}

#[no_mangle]
pub extern "C" fn free_vm_server(server_ptr: *mut VMServer) {
  if !server_ptr.is_null() {
    unsafe { let _ =Box::from_raw(server_ptr); }
  }
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
pub extern "C" fn free_c_string(ptr: *mut c_char) {
  if ptr.is_null() {
    return;
  }
  unsafe {
    let _ =CString::from_raw(ptr);
  }
}
