use uuid::Uuid;
use std::collections::HashMap;
use ocaml_sys::{caml_callback2, caml_copy_string, caml_named_value, Value};
use std::ffi::CString;
use serde::Serialize;
use serde_json::json;
use std::sync::mpsc::{Receiver, RecvTimeoutError};
use super::super::{jsonrpc, jsonrpc::JsonRpcResponse};
use super::event::Event;

#[derive(Serialize, Debug)]
pub struct OverlayedChangeSet {
  pub status: bool,
  pub message: String,
  pub vm_id: Uuid,
  pub parent_vm: Option<Uuid>,
  pub events: Vec<Event>,
  pub internal_transactions: Vec<InternalTransaction>,
}

#[derive(Debug, Serialize)]
pub struct InternalTransaction {
  pub sender: String,
  pub receiver: String,
  pub amount: i32,
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
  pub storage: HashMap<String, i32>,
  pub stack: Vec<i32>,
}

#[derive(Debug)]
pub enum Instruction {
  // stack Operations
  Push { value: i32 },
  Pop,

  // arithmetic Operations
  Add,
  Sub,
  Mul,
  Div,
  Mod,

  // comparison Operations
  Eq,
  Ne,
  Lt,
  Gt,
  Le,
  Ge,

  // bitwise Operations
  And,
  Or,
  Xor,
  Not,
  Shl { shift: u8 },
  Shr { shift: u8 },

  // storage Operations
  Set { key: String },
  Get { key: String },

  // control Flow Operations
  Jump { destination: usize },
  JumpIf { destination: usize },
  Halt,

  // call/message operations
  Call { address: String, gas_limit: i32, value: i32 },
  Transaction { sender: String, receiver: String, amount: i32 },
  Emit { event_name: String, data: Vec<i32> },
  Return,

  // environment operations
  GetBalance { address: String },
  GetCaller,
  GetCallValue,
  GetGasPrice,
  GetBlockNumber,
  GetBlockTimestamp,
}

// TODO: 
// - [ ] function make_internal_transaction() (get funds via RPC and return the tx_struct inside
//       layered_changeset)
// - [ ] nested contract calls

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
    let result = {
      println!("[VM] running: {} ; gas_limit: {}\n", self.id, self.transaction.gas_limit);

      let program = self.fetch_contract_code(self.transaction.receiver.clone());
      if let Err(err) = &program {
        (false, format!("Failed to fetch contract code: {}\n", err));
      }

      println!("[VM] fetched program: {}\n", program.as_ref().unwrap());

      let bytecode = self.parse_program_to_bytecode(program.as_ref().unwrap());
      if let Err(err) = &bytecode {
        (false, format!("Failed to parse program to bytecode: {}", err));
      }

      let instructions = self.decode_bytecode(bytecode.as_ref().unwrap());
      if let Err(err) = &instructions {
        (false, format!("Failed to decode bytecode: {}\n", err));
      }

      match self.execute_program(instructions.as_ref().unwrap(), self.transaction.gas_limit) {
        Ok(_) => (true, "Success".to_string()),
        Err(err) => (false, err),
      }
    };

    let change_set = OverlayedChangeSet {
      status: result.0,
      message: result.1,
      vm_id: self.id,
      parent_vm: None,
      events: vec!(),
      internal_transactions: vec!(),
    };

    // test event
    // change_set.events.push(Event {
    //   address: self.transaction.receiver.clone(),
    //   topics: vec!(),
    //   data: vec!(),
    // });

    vec![change_set]
  }

  pub fn parse_program_to_bytecode(&self, program: &str) -> Result<Vec<u8>, String> {
    let mut bytecode = Vec::new();
    let mut i = 0;

    let program: String = program.chars()
      .filter(|c| c.is_ascii_hexdigit())
      .collect();

    while i < program.len() {
      if i + 2 > program.len() {
        return Err("Program length must be even".to_string());
      }
      let byte_str = &program[i..i + 2];
      match u8::from_str_radix(byte_str, 16) {
        Ok(byte) => bytecode.push(byte),
        Err(_) => return Err(format!("Invalid byte format: {}", byte_str)),
      }
      i += 2;
    }
    Ok(bytecode)
  }

  pub fn decode_bytecode(&self, bytecode: &[u8]) -> Result<Vec<Instruction>, String> {
    let mut instructions = Vec::new();
    let mut i = 0;

    while i < bytecode.len() {
      match bytecode[i] {
        // stack Operations
        0x01 => {
          if i + 4 >= bytecode.len() {
            return Err("Push instruction expects a 4-byte value".to_string());
          }

          let value = i32::from_be_bytes([bytecode[i + 1], bytecode[i + 2], bytecode[i + 3], bytecode[i + 4]]);
          instructions.push(Instruction::Push { value });
          i += 5;
        }
        0x02 => {
          instructions.push(Instruction::Pop);
          i += 1;
        }

        // arithmetic Operations
        0x03 => {
          instructions.push(Instruction::Add);
          i += 1;
        }
        0x04 => {
          instructions.push(Instruction::Sub);
          i += 1;
        }
        0x05 => {
          instructions.push(Instruction::Mul);
          i += 1;
        }
        0x06 => {
          instructions.push(Instruction::Div);
          i += 1;
        }
        0x07 => {
          instructions.push(Instruction::Mod);
          i += 1;
        }

        // comparison Operations
        0x10 => {
          instructions.push(Instruction::Eq);
          i += 1;
        }
        0x11 => {
          instructions.push(Instruction::Ne);
          i += 1;
        }
        0x12 => {
          instructions.push(Instruction::Lt);
          i += 1;
        }
        0x13 => {
          instructions.push(Instruction::Gt);
          i += 1;
        }
        0x14 => {
          instructions.push(Instruction::Le);
          i += 1;
        }
        0x15 => {
          instructions.push(Instruction::Ge);
          i += 1;
        }

        // bitwise Operations
        0x20 => {
          instructions.push(Instruction::And);
          i += 1;
        }
        0x21 => {
          instructions.push(Instruction::Or);
          i += 1;
        }
        0x22 => {
          instructions.push(Instruction::Xor);
          i += 1;
        }
        0x23 => {
          instructions.push(Instruction::Not);
          i += 1;
        }
        0x24 => {
          if i + 1 >= bytecode.len() {
            return Err("Shl instruction expects a shift value".to_string());
          }
          let shift = bytecode[i + 1];
          instructions.push(Instruction::Shl { shift });
          i += 2;
        }
        0x25 => {
          if i + 1 >= bytecode.len() {
            return Err("Shr instruction expects a shift value".to_string());
          }
          let shift = bytecode[i + 1];
          instructions.push(Instruction::Shr { shift });
          i += 2;
        }

        // storage Operations
        0x30 => {
          if i + 33 > bytecode.len() {
            return Err("Set instruction expects a key length prefix and key string".to_string());
          }
          let key_len = bytecode[i + 1] as usize;
          if i + 2 + key_len > bytecode.len() {
            return Err("Key length exceeds remaining bytecode length".to_string());
          }
          let key = String::from_utf8(bytecode[i + 2..i + 2 + key_len].to_vec())
            .map_err(|_| "Failed to decode key as UTF-8".to_string())?;
          instructions.push(Instruction::Set { key });
          i += 2 + key_len;
        }
        0x31 => {
          if i + 33 > bytecode.len() {
            return Err("Get instruction expects a key length prefix and key string".to_string());
          }
          let key_len = bytecode[i + 1] as usize;
          if i + 2 + key_len > bytecode.len() {
            return Err("Key length exceeds remaining bytecode length".to_string());
          }
          let key = String::from_utf8(bytecode[i + 2..i + 2 + key_len].to_vec())
            .map_err(|_| "Failed to decode key as UTF-8".to_string())?;
          instructions.push(Instruction::Get { key });
          i += 2 + key_len;
        }

        // control Flow Operations
        0x40 => {
          if i + 4 >= bytecode.len() {
            return Err("Jump instruction expects a 4-byte destination".to_string());
          }
          let destination = usize::from_be_bytes([0, 0, 0, 0, bytecode[i + 1], bytecode[i + 2], bytecode[i + 3], bytecode[i + 4]]);
          instructions.push(Instruction::Jump { destination });
          i += 5;
        }
        0x41 => {
          if i + 4 >= bytecode.len() {
            return Err("JumpIf instruction expects a 4-byte destination".to_string());
          }
          let destination = usize::from_be_bytes([0, 0, 0, 0, bytecode[i + 1], bytecode[i + 2], bytecode[i + 3], bytecode[i + 4]]);
          instructions.push(Instruction::JumpIf { destination });
          i += 5;
        }
        0x42 => {
          instructions.push(Instruction::Halt);
          i += 1;
        }

        // call/Message Operations
        0x50 => {
          if i + 9 >= bytecode.len() {
            return Err("Call instruction expects address, gas limit, and value".to_string());
          }
          let address = String::from_utf8(bytecode[i + 1..i + 5].to_vec()).map_err(|_| "Invalid address".to_string())?;
          let gas_limit = i32::from_be_bytes([bytecode[i + 5], bytecode[i + 6], bytecode[i + 7], bytecode[i + 8]]);
          let value = i32::from_be_bytes([bytecode[i + 9], bytecode[i + 10], bytecode[i + 11], bytecode[i + 12]]);
          instructions.push(Instruction::Call { address, gas_limit, value });
          i += 13;
        },
        0x70 => {
          // ensure there's enough data for the event name length and at least one data entry
          if i + 2 >= bytecode.len() {
            return Err("Emit instruction expects an event name length prefix and event data".to_string());
          }

          // get event name length
          let name_len = bytecode[i + 1] as usize;
          if i + 2 + name_len > bytecode.len() {
            return Err("Event name length exceeds remaining bytecode length".to_string());
          }

          // extract event name
          let event_name = String::from_utf8(bytecode[i + 2..i + 2 + name_len].to_vec())
            .map_err(|_| "Failed to decode event name as UTF-8".to_string())?;
          i += 2 + name_len;

          // now extract data (let's assume data is a series of 4-byte integers)
          let mut data = Vec::new();
          while i + 4 <= bytecode.len() {
            let value = i32::from_be_bytes([bytecode[i], bytecode[i + 1], bytecode[i + 2], bytecode[i + 3]]);
            data.push(value);
            i += 4;
          }

          instructions.push(Instruction::Emit { event_name, data });
        },
        0x51 => {
          if i + 8 >= bytecode.len() {
            return Err("Transaction instruction expects sender, receiver, and amount".to_string());
          }
          let sender = String::from_utf8(bytecode[i + 1..i + 5].to_vec()).map_err(|_| "Invalid sender".to_string())?;
          let receiver = String::from_utf8(bytecode[i + 5..i + 9].to_vec()).map_err(|_| "Invalid receiver".to_string())?;
          let amount = i32::from_be_bytes([bytecode[i + 9], bytecode[i + 10], bytecode[i + 11], bytecode[i + 12]]);
          instructions.push(Instruction::Transaction { sender, receiver, amount });
          i += 13;
        }
        0x52 => {
          instructions.push(Instruction::Return);
          i += 1;
        }

        // environment Operations
        0x60 => {
          if i + 4 > bytecode.len() {
            return Err("GetBalance expects an address".to_string());
          }
          let address = String::from_utf8(bytecode[i + 1..i + 5].to_vec()).map_err(|_| "Invalid address".to_string())?;
          instructions.push(Instruction::GetBalance { address });
          i += 5;
        }
        0x61 => {
          instructions.push(Instruction::GetCaller);
          i += 1;
        }
        0x62 => {
          instructions.push(Instruction::GetCallValue);
          i += 1;
        }
        0x63 => {
          instructions.push(Instruction::GetGasPrice);
          i += 1;
        }
        0x64 => {
          instructions.push(Instruction::GetBlockNumber);
          i += 1;
        }
        0x65 => {
          instructions.push(Instruction::GetBlockTimestamp);
          i += 1;
        }

        _ => {
          return Err(format!("Unknown opcode: 0x{:02x}", bytecode[i]));
        }
      }
    }

    Ok(instructions)
  }

  fn execute_program(&mut self, program: &[Instruction], gas_limit: i32) -> Result<(), String> {
    let mut gas_limit = gas_limit;

    for opcode in program {
      let gas_cost = Self::get_instruction_gas_cost(opcode);
      if gas_limit < gas_cost {
        return Err("Out of gas".to_string())
      }

      gas_limit = gas_limit - gas_cost;
      self.execute_instruction(opcode)?;
    }
    Ok(())
  }

  fn get_instruction_gas_cost(instruction: &Instruction) -> i32 {
    match instruction {
      // arithmetic Operations
      Instruction::Add | Instruction::Sub | Instruction::Mul 
        | Instruction::Div | Instruction::Mod => 3,

      // stack Operations
      Instruction::Push { .. } | Instruction::Pop => 2,

      // comparison Operations
      Instruction::Eq | Instruction::Ne | Instruction::Lt 
        | Instruction::Gt | Instruction::Le | Instruction::Ge => 4,

      // bitwise Operations
      Instruction::And | Instruction::Or | Instruction::Xor | Instruction::Not => 3,
      Instruction::Shl { .. } | Instruction::Shr { .. } => 4,

      // storage Operations
      Instruction::Set { .. } | Instruction::Get { .. } => 5,

      // control Flow Operations
      Instruction::Jump { .. } | Instruction::JumpIf { .. } => 8,
      Instruction::Halt => 1,

      // call/Message Operations
      Instruction::Call { .. } => 10,
      Instruction::Transaction { .. } => 12,
      Instruction::Emit { .. } => 10,
      Instruction::Return => 5,

      // environment Operations
      Instruction::GetBalance { .. } => 6,
      Instruction::GetCaller => 4,
      Instruction::GetCallValue => 4,
      Instruction::GetGasPrice => 4,
      Instruction::GetBlockNumber => 4,
      Instruction::GetBlockTimestamp => 4,
    }
  }

  // TODO: write all instructions
  fn execute_instruction(&mut self, instruction: &Instruction) -> Result<(), String> {
    match instruction {
      Instruction::Set { key } => {
        if let Some(val) = self.stack.pop() {
          println!("[VM] SET {} {}", key, val);
          Ok(())
        } else {
          Err("[VM] SET failed: stack is empty".to_string())
        }
      }
      Instruction::Get { key } => {
        if let Some(value) = self.storage.get(key) {
          self.stack.push(*value);
          println!("[VM] GET {} => {}", key, value);
          Ok(())
        } else {
          Err(format!("[VM] GET failed: key '{}' not found", key))
        }
      }
      Instruction::Push { value } => {
        self.stack.push(*value);
        println!("[VM] PUSH {}", value);
        Ok(())
      }
      Instruction::Pop => {
        if let Some(value) = self.stack.pop() {
          println!("[VM] POP {}", value);
          Ok(())
        } else {
          Err("[VM] POP failed: stack is empty".to_string())
        }
      }
      Instruction::Add => {
        if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
          let result = a + b;
          self.stack.push(result);
          println!("[VM] ADD {} + {} = {}", b, a, result);
          Ok(())
        } else {
          Err("[VM] ADD failed: insufficient operands on stack".to_string())
        }
      }
      Instruction::Sub => {
        if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
          let result = b - a;
          self.stack.push(result);
          println!("[VM] SUB {} - {} = {}", b, a, result);
          Ok(())
        } else {
          Err("[VM] SUB failed: insufficient operands on stack".to_string())
        }
      }
      Instruction::Mul => {
        if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
          let result = a * b;
          self.stack.push(result);
          println!("[VM] MUL {} * {} = {}", b, a, result);
          Ok(())
        } else {
          Err("[VM] MUL failed: insufficient operands on stack".to_string())
        }
      }
      Instruction::Div => {
        if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
          if a == 0 {
            println!("[VM] DIV failed: division by zero");
            self.stack.push(0);
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
      _ => {
        println!("[VM] Unimplemented opcode");
        Ok(())
      }
    }
  }

  fn make_transaction(&self, receiver: &str, amount: i32) -> Result<InternalTransaction, String> {
    let sender = self.transaction.receiver.clone();

    let req = jsonrpc::serialize_request(
      "get_account",
      json!([sender])
      );

    let res = self.json_rpc(req).unwrap();
    let balance = res.result.unwrap()["balance"].take().as_i64().unwrap();

    if (balance as i32) < (amount as i32) {
      return Err("Insufficient balance".to_string())
    } else {
      Ok(InternalTransaction {
        sender,
        receiver: receiver.to_string(),
        amount
      })
    }
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


  // internal function to make JSON-RPC request
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

  // internal function to call OCaml callback (for JSON-RPC calls) 
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
