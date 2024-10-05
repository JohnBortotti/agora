use uuid::Uuid;
use ocaml_sys::{caml_callback2 , caml_copy_string, caml_named_value, Value};
use std::ffi::CString;
use std::sync::mpsc::{Receiver, RecvTimeoutError};
use super::super::{jsonrpc, jsonrpc::JsonRpcResponse};
use serde_json::json;

#[derive(Debug, Clone)]
pub struct Transaction {
  pub hash: String,
  pub sender: String,
  pub receiver: String,
  pub amount: i32,
  pub gas_limit: i32,
  pub gas_price: i32,
  pub nonce: i32,
  pub payload: String,
  pub signature: String,
}

pub struct VM {
  pub id: Uuid,
  pub rx: Receiver<String>,
  pub transaction: Transaction,
}

impl VM {
  pub fn new(id: Uuid, rx: Receiver<String>, transaction: Transaction) -> Self {
    VM {
      id,
      rx,
      transaction
    }
  }

  // TODO:
  // returns OK<_> | Err<_> :
  //    - return writes made on chain (transactions and storage commit)
  //    - return gas used
  pub fn run(&self) {
    println!("[VM] running: {}\n", self.id);

    let account_address = self.transaction.sender.to_string();
    let request_json = jsonrpc::serialize_request(
      "get_account",
      json!([account_address])
    );
    let response = self.json_rpc(request_json).unwrap();
    let code = response.result.unwrap();

    println!("\n[VM] code: {:?}\n", code);

    println!("\n[VM] finished execution: {}\n", self.id);
  }

  pub fn json_rpc(&self, json: String) -> Result<JsonRpcResponse, String> {
    println!("\n[VM] {} is making a JSON-RPC request: {}\n", self.id, json);

    Self::call_ocaml_callback(&self.id.to_string(), &json);

    match self.rx.recv_timeout(std::time::Duration::from_secs(12)) {
      Ok(message) => {
        println!("[VM] Received message: {}", message);
        return match jsonrpc::deserialize_response(&message) {
          Ok(res) => Ok(res),
          _ => Err("message deserialize error".to_string())
        }
      }
      Err(RecvTimeoutError::Timeout) => {
        println!("[VM] No message received within timeout. Continuing...");
        return Err("message timeout".to_string())
      }
      Err(RecvTimeoutError::Disconnected) => {
        println!("[VM] Channel disconnected. Shutting down VM.");
        return Err("channel disconnected".to_string())
      }
    };

  }

  pub fn call_ocaml_callback(vm_id: &str, json: &str) {
    let vm_id_cstring = CString::new(vm_id).expect("Failed to create CString for vm_id");
    let json_cstring = CString::new(json).expect("Failed to create CString for method");

    unsafe {
      let vm_id_value: Value = caml_copy_string(vm_id_cstring.as_ptr());
      let json_value: Value = caml_copy_string(json_cstring.as_ptr());

      let ocaml_callback_str = CString::new("ocaml_json_rpc_callback").unwrap();
      let ocaml_callback_fn: *const Value =
        caml_named_value(ocaml_callback_str.as_ptr());

      if !ocaml_callback_fn.is_null() {
        caml_callback2(*ocaml_callback_fn, vm_id_value, json_value);
      } else {
        eprintln!("OCaml callback not found");
      }
    }
  }
}
