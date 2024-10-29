mod jsonrpc;
mod vm;

use vm::server::Server;
use ethnum::U256;
use vm::vm_core::Transaction;
use std::ffi::{CStr, CString, c_char, c_void};
use uuid::Uuid;

#[no_mangle]
pub extern "C" fn ffi_create_vm_server() -> *mut Server {
  let server = Server::new();
  Box::into_raw(Box::new(server))
}

#[no_mangle]
pub extern "C" fn ffi_spawn_vm(
  server_ptr: *mut Server,
  hash: *const c_char,
  sender: *const c_char,
  receiver: *const c_char,
  amount: *const c_char, // passed as a hex string
  gas_limit: *const c_char, // passed as a hex string
  gas_price: *const c_char, // passed as a hex string
  nonce: *const c_char, // passed as a hex string
  payload: *const c_char,
  signature: *const c_char,
  ) -> *mut c_char {
  let server = unsafe {
    assert!(!server_ptr.is_null());
    &mut *server_ptr
  };

  let hash = unsafe {
    if hash.is_null() {
      eprintln!("Error: hash pointer is null");
      return std::ptr::null_mut();
    }
    CStr::from_ptr(hash).to_string_lossy().into_owned()
  };

  let sender = unsafe {
    if sender.is_null() {
      eprintln!("Error: sender pointer is null");
      return std::ptr::null_mut();
    }
    CStr::from_ptr(sender).to_string_lossy().into_owned()
  };

  let receiver = unsafe {
    if receiver.is_null() {
      eprintln!("Error: receiver pointer is null");
      return std::ptr::null_mut();
    }
    CStr::from_ptr(receiver).to_string_lossy().into_owned()
  };

  let payload = unsafe {
    if payload.is_null() {
      eprintln!("Error: payload pointer is null");
      return std::ptr::null_mut();
    }
    CStr::from_ptr(payload).to_string_lossy().into_owned()
  };

  let signature = unsafe {
    if signature.is_null() {
      eprintln!("Error: signature pointer is null");
      return std::ptr::null_mut();
    }
    CStr::from_ptr(signature).to_string_lossy().into_owned()
  };

  let amount_str = unsafe { CStr::from_ptr(amount).to_string_lossy() };
  let amount = U256::from_str_hex(&amount_str).expect("Invalid amount");

  let gas_limit_str = unsafe { CStr::from_ptr(gas_limit).to_string_lossy() };
  let gas_limit = U256::from_str_hex(&gas_limit_str).expect("Invalid gas limit");

  let gas_price_str = unsafe { CStr::from_ptr(gas_price).to_string_lossy() };
  let gas_price = U256::from_str_hex(&gas_price_str).expect("Invalid gas price");

  let nonce_str = unsafe { CStr::from_ptr(nonce).to_string_lossy() };
  let nonce = U256::from_str_hex(&nonce_str).expect("Invalid nonce");

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

  let res = server.run(transaction);
  let res_string = res.to_string();

  match CString::new(res_string) {
    Ok(c_string) => c_string.into_raw(),
    Err(_) => {
      eprintln!("Error: Failed to convert VM ID to CString");
      std::ptr::null_mut()
    }
  }
}

#[no_mangle]
pub extern "C" fn ffi_send_data_to_vm(
  server_ptr: *mut Server,
  request_id: *const c_char,
  data: *const c_char) {
  let server = unsafe {
    if server_ptr.is_null() {
      eprintln!("Error: server_ptr is null");
      return;
    }
    &mut*server_ptr
  };

  let request_id_str = unsafe {
    if request_id.is_null() {
      eprintln!("Error: request_id pointer is null");
      return;
    }
    CStr::from_ptr(request_id).to_str().unwrap()
  };

  let data_str = unsafe {
    if data.is_null() {
      eprintln!("Error: data pointer is null");
      return;
    }
    CStr::from_ptr(data).to_str().unwrap()
  };

  match Uuid::parse_str(request_id_str) {
    Ok(request_id) => server.send_data_to_vm(request_id, data_str.to_string()),
    Err(_) => eprintln!("Error: Invalid UUID format for vm_id"),
  }
}

#[no_mangle]
pub extern "C" fn ffi_destroy_vm_server(server_ptr: *mut c_void) {
  if !server_ptr.is_null() {
    unsafe {
      let server = Box::from_raw(server_ptr as *mut Server);
      drop(server);
    }
  }
}
