mod server;
mod vm_core;

use std::collections::HashMap;
use super::u256::U256;

use std::ffi::{CStr, CString};
use ocaml_sys::{caml_callback, caml_named_value, Value, caml_copy_string};
use serde_json::{json, Value as JsonValue};

#[derive(Debug, Clone, Copy)]
pub enum Opcode {
  Push(U256),
  Add,
  Sub,
  Mul,
  Div, 
  Store(u8),
  Load(u8),
  Jump(usize),
  JumpIfZero(usize),
  Halt,
}

fn get_gas_cost(opcode: &Opcode) -> u64 {
  match opcode {
    Opcode::Push(_) => 2,
    Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div => 3,
    Opcode::Store(_) | Opcode::Load(_) => 5,
    Opcode::Jump(_) | Opcode::JumpIfZero(_) => 8,
    Opcode::Halt => 1,
  }
}

#[derive(Debug)]
pub struct Transaction {
  pub sender: String,
  pub receiver: String,
  pub amount: u128,
  pub gas_limit: u128,
  pub gas_price: u128,
  pub payload: String
}

pub struct VM {
  tx: Transaction,
  stack: Vec<U256>,
  storage: HashMap<u8, U256>,
  program_counter: usize,
  instructions: Vec<Opcode>,
  gas: u64,
}

impl VM {
  pub fn new(tx: Transaction, gas: u64) -> Self {
    println!("[VM] starting up vm...");
    println!("[VM] fetching contract bytecode...\n");

    let program = Self::fetch_program(&tx.receiver);

    VM {
      tx,
      stack: Vec::new(),
      storage: HashMap::new(),
      program_counter: 0,
      instructions: program,
      gas,
    }
  }

  pub fn run(&mut self) {
    while self.program_counter < self.instructions.len() {
      let instruction = self.instructions[self.program_counter];
      let cost = get_gas_cost(&instruction);

      println!("executing opcode: {:?}", instruction);
      println!("cost: {}", cost);

      if self.gas < cost {
        println!("Out of gas! Execution halted.");
        break;
      }

      self.gas -= cost;
      self.program_counter += 1;
      self.execute(instruction);
    }
  }

  fn execute(&mut self, instruction: Opcode) {
    match instruction {
      Opcode::Push(value) => self.stack.push(value),
      Opcode::Add => self.binary_op(|a, b| (a + b)),
      Opcode::Sub => self.binary_op(|a, b| (a - b)),
      Opcode::Mul => self.binary_op(|a, b| (a * b)),
      Opcode::Div => self.binary_op(|a, b| (a / b)),
      Opcode::Store(address) => {
        if let Some(value) = self.stack.pop() {
          self.storage.insert(address, value);
        }
      }
      Opcode::Load(address) => {
        if let Some(&value) = self.storage.get(&address) {
          self.stack.push(value);
        }
      }
      Opcode::Jump(addr) => self.program_counter = addr,
      Opcode::JumpIfZero(addr) => {
        if let Some(&value) = self.stack.last() {
          if value == U256(0, 0) {
            self.program_counter = addr;
          }
        }
      }
      Opcode::Halt => self.program_counter = self.instructions.len(),
    }
  }

  fn binary_op<F>(&mut self, operation: F)
    where
      F: Fn(U256, U256) -> U256,
    {
      if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
        self.stack.push(operation(b, a));
      }
    }

  fn jsonrpc_request_with_polling(method: &str, params: Vec<JsonValue>) -> Result<String, String> {
    unsafe {
      let callback_name = CString::new("ocaml_jsonrpc_callback")
        .map_err(|e| format!("Failed to create CString for callback: {}", e))?;
      let callback: *const Value = caml_named_value(callback_name.as_ptr());
      if callback.is_null() {
        return Err("OCaml callback 'ocaml_jsonrpc_callback' not found".to_string());
      }

      let request = json!({
        "jsonrpc": "2.0",
        "method": method,
        "params": params,
        "id": 1 
      });

      let request_str = request.to_string();
      println!("Sending JSON-RPC request: {}", request_str);

      let request_cstring = CString::new(request_str)
        .map_err(|e| format!("Failed to create CString for request: {}", e))?;
      let ocaml_request = caml_copy_string(request_cstring.as_ptr());

      let result: Value = caml_callback(*callback, ocaml_request);

      std::thread::sleep(std::time::Duration::from_millis(500));

      let result_cstr = CStr::from_ptr(ocaml_sys::string_val(result) as *const i8);

      let result_str = match result_cstr.to_str() {
        Ok(valid_str) => valid_str.to_string(),
        Err(err) => panic!("Error decoding UTF-8: {}", err)
      };

      println!("Initial response from OCaml: {}", result_str);

      if result_str == "Request is being processed" {
        loop {
          std::thread::sleep(std::time::Duration::from_millis(500));

          let poll_result = Self::poll_for_result("1");
          println!("{}", poll_result);

          if poll_result != "Still processing" {
            println!("Final result: {}", poll_result);
            return Ok(poll_result);
          }

          println!("Still waiting for the result...");
        }
      } else {
        Ok(result_str.to_string())
      }
    }
  }

  fn poll_for_result(id: &str) -> String {
    unsafe {
      let callback_name = CString::new("ocaml_poll_callback")
        .expect("Failed to create CString for poll callback");
      let callback: *const Value = caml_named_value(callback_name.as_ptr());
      if callback.is_null() {
        panic!("OCaml poll callback 'ocaml_poll_callback' not found");
      }

      let request = json!({
        "jsonrpc": "2.0",
        "method": "poll_result",
        "params": [id]
      });

      let request_str = request.to_string();
      println!("Polling for result with ID {}: {}", id, request_str);

      let request_cstring = CString::new(request_str).unwrap();
      let ocaml_request = caml_copy_string(request_cstring.as_ptr());

      let result: Value = caml_callback(*callback, ocaml_request);

      let result_str = CStr::from_ptr(ocaml_sys::string_val(result) as *const i8).to_str().unwrap();
      result_str.to_string()
    }
  }

  fn fetch_program(contract_address: &str) -> Vec<Opcode> {
    let method = "get_account";
    let params = vec![json!(contract_address)];
    let result_str = Self::jsonrpc_request_with_polling(method, params);

    println!("[VM] ocaml response: {:?}", result_str);

    let res = match result_str {
      Err(_) => vec!(),
      Ok(_) => vec!()
    };

    res
  }
}
