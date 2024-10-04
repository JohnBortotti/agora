use uuid::Uuid;
use super::server::VMTable;
use ocaml_sys::{caml_callback3, caml_copy_string, caml_named_value, Value};
use std::ffi::CString;

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

#[derive(Debug, Clone)]
pub enum VMStatus {
  Running,
  WaitingForData,
}

pub struct VMState {
  pub status: VMStatus,
  pub parent_vm: Option<Uuid>,
  pub data: Vec<(String, String)>,
  pub requested_key: Option<String>,
}

pub struct VM {
  pub id: Uuid,
  pub vm_table: VMTable,
  pub transaction: Transaction,
}

impl VM {
  pub fn new(id: Uuid, vm_table: VMTable, transaction: Transaction) -> Self {
    VM {
      id,
      vm_table,
      transaction
    }
  }

  pub fn run(&self) {
    println!("[VM] running: {}\n", self.id);

    // TODO: fetch contract code

    let account_address = "0x12345abcde".to_string();

    self.json_rpc(
      "get_account_info".to_string(),
      format!(r#"{{"account": "{}"}}"#, account_address),
      );

    println!("\n[VM] finished execution: {}\n", self.id);
  }

  // TODO: set status::running after receive response
  // TODO: after getting the response, remove the data from guard.data
  pub fn json_rpc(&self, method: String, params: String) {
    println!("\n[VM] {} is making a JSON-RPC request: {} ; {}\n", self.id, method, params);

    {
      let mut vms_guard = self.vm_table.lock().unwrap();
      let vm_state = vms_guard.get_mut(&self.id).unwrap();
      let mut state_guard = vm_state.lock().unwrap();
      state_guard.status = VMStatus::WaitingForData;
      state_guard.requested_key = Some(method.clone());
    }

    Self::call_ocaml_callback(&self.id.to_string(), &method, &params);

    loop {
      std::thread::sleep(std::time::Duration::from_millis(200));

      let vms_guard = self.vm_table.lock().unwrap();
      let vm_state = vms_guard.get(&self.id).unwrap();
      let state_guard = vm_state.lock().unwrap();

      if !state_guard.data.is_empty() {
        for (key, value) in &state_guard.data {
          println!("[VM] {} received JSON-RPC response for {}: {}\n", self.id, key, value);
        }
        break;
      } else {}
    }
  }

  pub fn call_ocaml_callback(vm_id: &str, method: &str, params: &str) {
    let vm_id_cstring = CString::new(vm_id).expect("Failed to create CString for vm_id");
    let method_cstring = CString::new(method).expect("Failed to create CString for method");
    let params_cstring = CString::new(params).expect("Failed to create CString for params");

    unsafe {
      let vm_id_value: Value = caml_copy_string(vm_id_cstring.as_ptr());
      let method_value: Value = caml_copy_string(method_cstring.as_ptr());
      let params_value: Value = caml_copy_string(params_cstring.as_ptr());

      let ocaml_callback_str = CString::new("ocaml_json_rpc_callback").unwrap();
      let ocaml_callback_fn: *const Value =
        caml_named_value(ocaml_callback_str.as_ptr());

      if !ocaml_callback_fn.is_null() {
        caml_callback3(*ocaml_callback_fn, vm_id_value, method_value, params_value);
      } else {
        eprintln!("OCaml callback not found");
      }
    }
  }
}
