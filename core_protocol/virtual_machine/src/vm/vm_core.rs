use uuid::Uuid;
use std::collections::HashMap;
use super::super::u256::U256;
use ocaml_sys::{caml_callback2, caml_copy_string, caml_named_value, Value};
use std::ffi::CString;
use serde::Serialize;
use serde_json::json;
use std::sync::mpsc::{Receiver, RecvTimeoutError};
use super::super::{jsonrpc, jsonrpc::JsonRpcResponse};
use super::log::Log;

#[derive(Serialize, Debug)]
pub struct OverlayedChangeSet {
  pub vm_id: Uuid,
  pub logs: Vec<Log>,
}

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
  pub storage: HashMap<String, U256>,
  pub stack: Vec<U256>,
}

#[derive(Debug)]
pub enum Opcode {
  Set { key: String },
  Get { key: String },
  Push { value: U256 },
  Pop,
  Add,
  Sub,
  Mul,
  Div,
}

impl VM {
  pub fn new(id: Uuid, rx: Receiver<String>, transaction: Transaction) -> Self {
    VM {
      id,
      rx,
      transaction,
      storage: HashMap::new(),
      stack: vec!()
    }
  }

  pub fn run(&mut self) -> Vec<OverlayedChangeSet> {
    println!("[VM] running: {}\n", self.id);
    let program = self.fetch_contract_code(self.transaction.receiver.clone()).unwrap();
    println!("[VM] fetched program: {}\n", program);
    
    let code = vec![
      Opcode::Push { value: U256::from(10 as u32) },
      Opcode::Push { value: U256::from(20 as u32) },
      Opcode::Add,
      Opcode::Set {
        key: "result".to_string(),
      },
      Opcode::Get {
        key: "result".to_string(),
      },
    ];

    let _ = self.execute_program(&code, U256::from(self.transaction.gas_limit as u32));

    let mut change_set = OverlayedChangeSet { vm_id: self.id, logs: vec!() };
    change_set.logs.push(Log {
      address: self.transaction.receiver.clone(),
      topics: vec!(),
      data: vec!(),
    });

    vec![change_set]
  }

  fn fetch_contract_code(&self, contract_address: String) -> Result<String, String> {
    let req = jsonrpc::serialize_request(
      "get_code",
      json!([contract_address])
    );
    let res = self.json_rpc(req).unwrap();
    let code = res.result.unwrap()["code"].take();
    Ok(code.to_string())
  }

  fn execute_program(&mut self, program: &[Opcode], gas_limit: U256) -> Result<(), String> {
    let mut gas_limit = gas_limit;

    for opcode in program {
      let gas_cost = self.get_gas_cost(opcode);
      if gas_limit < gas_cost {
        return Err("Out of gas".to_string())
      }

      gas_limit = gas_limit - gas_cost;
      self.execute_opcode(opcode)?;
    }
    Ok(())
  }

  fn get_gas_cost(&self, opcode: &Opcode) -> U256 {
    match opcode {
      Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div => 3u32.into(),
      Opcode::Push { .. } | Opcode::Pop => 2u32.into(),
      Opcode::Set { .. } | Opcode::Get { .. } => 5u32.into(),
    }
  }

  fn execute_opcode(&mut self, opcode: &Opcode) -> Result<(), String> {
    match opcode {
      Opcode::Set { key } => {
        if let Some(val) = self.stack.pop() {
          println!("[VM] SET {} {}", key, val);
          Ok(())
        } else {
          Err("[VM] SET failed: stack is empty".to_string())
        }
      }
      Opcode::Get { key } => {
        if let Some(value) = self.storage.get(key) {
          self.stack.push(*value);
          println!("[VM] GET {} => {}", key, value);
          Ok(())
        } else {
          Err(format!("[VM] GET failed: key '{}' not found", key))
        }
      }
      Opcode::Push { value } => {
        self.stack.push(*value);
        println!("[VM] PUSH {}", value);
        Ok(())
      }
      Opcode::Pop => {
        if let Some(value) = self.stack.pop() {
          println!("[VM] POP {}", value);
          Ok(())
        } else {
          Err("[VM] POP failed: stack is empty".to_string())
        }
      }
      Opcode::Add => {
        if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
          let result = a + b;
          self.stack.push(result);
          println!("[VM] ADD {} + {} = {}", b, a, result);
          Ok(())
        } else {
          Err("[VM] ADD failed: insufficient operands on stack".to_string())
        }
      }
      Opcode::Sub => {
        if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
          let result = b - a;
          self.stack.push(result);
          println!("[VM] SUB {} - {} = {}", b, a, result);
          Ok(())
        } else {
          Err("[VM] SUB failed: insufficient operands on stack".to_string())
        }
      }
      Opcode::Mul => {
        if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
          let result = a * b;
          self.stack.push(result);
          println!("[VM] MUL {} * {} = {}", b, a, result);
          Ok(())
        } else {
          Err("[VM] MUL failed: insufficient operands on stack".to_string())
        }
      }
      Opcode::Div => {
        if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
          if a == U256::zero() {
            println!("[VM] DIV failed: division by zero");
            self.stack.push(U256::zero());
            Err("[VM] DIV failed: division by zero".to_string())
          } else {
            let result = b / a;
            self.stack.push(result);
            println!("[VM] DIV {} / {} = {}", b, a, result);
            Ok(())
          }
        } else {
          Err("[VM] DIV failed: insufficient operands on stack".to_string())
        }
      }
    }
  }

  fn json_rpc(&self, json: String) -> Result<JsonRpcResponse, String> {
    println!("\n[VM] {} is making a JSON-RPC request: {}\n", self.id, json);

    Self::call_ocaml_callback(&self.id.to_string(), &json);

    match self.rx.recv_timeout(std::time::Duration::from_secs(20)) {
      Ok(message) => {
        println!("[VM] Received message: {}", message);
        return match jsonrpc::deserialize_response(&message) {
          Ok(res) => Ok(res),
          _ => Err("message deserialize error".to_string())
        }
      }
      Err(RecvTimeoutError::Timeout) => {
        println!("[VM] No message received within timeout.");
        return Err("message timeout".to_string())
      }
      Err(RecvTimeoutError::Disconnected) => {
        println!("[VM] Channel disconnected. Shutting down VM.");
        return Err("channel disconnected".to_string())
      }
    };
  }

  fn call_ocaml_callback(vm_id: &str, json: &str) {
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
