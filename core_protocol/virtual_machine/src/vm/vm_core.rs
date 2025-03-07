use uuid::Uuid;
use ethnum::U256;
use std::collections::HashMap;
use serde::{Serialize, Deserialize};
use super::event::Event;
use super::instruction;
use super::instruction::Instruction;
use crate::jsonrpc;
use serde_json::json;
use crate::jsonrpc::{deserialize_response, JsonRpcRequest};
use sha2::Digest;

pub trait VMStateMachine {
  fn poll(&mut self) -> VMStatus;
  fn supply_data(&mut self, request_id: Uuid, data: String);
  fn get_id(&self) -> Uuid;
  fn get_contract_address(&self) -> String;
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum VMStatus {
  Initializing { request_id: Uuid },
  Running,
  Pending { request_id: Uuid, event: VMEvent },
  ReceivedData { request_id: Uuid },
  Finished { change_set: OverlayedChangeSet },
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum VMEvent {
  SpawnVM(Transaction),
  RequestData(JsonRpcRequest),
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct OverlayedChangeSet {
  pub status: bool,
  pub message: String,
  pub vm_id: Uuid,
  pub events: Vec<Event>,
  pub internal_transactions: Vec<InternalTransaction>,
  pub gas_limit: U256,
  pub gas_used: U256,
  pub gas_remaining: U256,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub struct InternalTransaction {
  pub sender: String,
  pub receiver: String,
  pub amount: U256,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Transaction {
  pub hash: String,
  pub sender: String,
  pub receiver: String,
  pub amount: U256,
  pub gas_limit: U256,
  pub gas_price: U256,
  pub nonce: U256,
  pub payload: String,
  pub signature: String,
}

#[derive(Debug, Clone)]
pub struct VM {
  pub id: Uuid,
  pub transaction: Transaction,
  pub storage: HashMap<String, U256>,
  pub stack: Vec<U256>,
  pub memory: Vec<u8>,
  pub pc: usize,
  pub instructions: Option<Vec<Instruction>>,
  pub state: VMStatus,
  pub overlayed_changeset: OverlayedChangeSet,
  pub supplied_data: HashMap<Uuid, String>,
}

// examples:
//
// // SUM 
// 01                                                                // push
// 000000000000000000000000000000000000000000000000000000000000000a  // 
// 01                                                                // push
// 0000000000000000000000000000000000000000000000000000000000000014  // push
// 03                                                                // sum
//
// // EMIT EVENT
// 70                                                                // emit
// 00 08                                                             // event_name length
// 54 72 61 6e 73 66 65 72                                           // UTF-8 encoded "Transfer"
// 0100000000000000000000000000000000000000000000000000000000000000  // Topic 1
// 0200000000000000000000000000000000000000000000000000000000000000  // Topic 2
// 0300000000000000000000000000000000000000000000000000000000000000  // Topic 3
// 00 02                                                             // data lenght (32 bytes)
// aa aa                                                             // Event Data
//
// // TRANSACTION
// 51                                                                // transaction
// 0000000000000000000000000000000000000000000000000000000000000001  // receiver
// 0000000000000000000000000000000000000000000000000000000000000005  // amount
//
// // CALL CONTRACT
// 50
// 0000000000000000000000000000000000000000000000000000000000000001  // contract address
// 0000000000000000000000000000000000000000000000000000000000000001  // gas limit
// 0000000000000000000000000000000000000000000000000000000000000000  // amount
// 00 00                                                             // payload lenght (32 bytes)

impl VMStateMachine for VM {
  fn poll(&mut self) -> VMStatus {
    match &self.state {
      VMStatus::Initializing { request_id } => {
        match self.supplied_data.get(request_id) {
          Some(data) => {
            let res = deserialize_response(data).unwrap().result.unwrap()["code"].take();
            // println!("[VM] Data found: {}", res);
            let instructions = Self::decode_program(res.to_string().as_str()).unwrap();
            self.instructions = Some(instructions);
            self.state = VMStatus::Running;
            return self.state.clone();
          }
          None => {
            self.state = VMStatus::Initializing { request_id: request_id.clone() };
            return self.state.clone();
          }
        }}
      VMStatus::Running | VMStatus::ReceivedData { .. } => {
        match self.execute_next_instruction() {
          Ok(event) => {
            match event {
              Some(event) => {
                self.state = VMStatus::Pending { request_id: Uuid::new_v4(), event: event.clone() };
                self.state.clone()
              }
              None => VMStatus::Running
            }
          }
          Err(err) => {
            self.overlayed_changeset.status = false;
            self.overlayed_changeset.message = err;
            self.state = VMStatus::Finished {
              change_set: self.overlayed_changeset.clone()
            };
            self.state.clone()
          }
        }
      }
      VMStatus::Pending { request_id: _, event } => {
        match event {
          | VMEvent::SpawnVM(_) 
          | VMEvent::RequestData(_) => {
            self.state.clone()
          }
        }
      }
      VMStatus::Finished { change_set } => {
        VMStatus::Finished { change_set: change_set.clone() }
      }
    }
  }

  fn supply_data(&mut self, request_id: Uuid, data: String) {
    // println!("[VM] Data supplied, id: {}", request_id);
    self.supplied_data.insert(request_id, data);

    match self.state {
      VMStatus::Initializing { .. } => {
        self.state = VMStatus::Initializing { request_id }
      }
      _ => {
        self.state = VMStatus::ReceivedData { request_id };
      }
    }
  }

  fn get_id(&self) -> Uuid {
    self.id
  }

  fn get_contract_address(&self) -> String {
    self.transaction.receiver.clone()
  }
}

impl VM {
  pub fn new(id: Uuid, transaction: Transaction) -> Self {
    let initial_change_set = OverlayedChangeSet {
      status: true,
      message: "Success".to_string(),
      vm_id: id,
      events: vec![],
      internal_transactions: vec![],
      gas_limit: transaction.gas_limit,
      gas_remaining: transaction.gas_limit,
      gas_used: U256::from(0 as u64),
    };

    VM {
      id,
      transaction: transaction.clone(),
      storage: HashMap::new(),
      stack: vec!(),
      memory: vec!(),
      pc: 0,
      instructions: None,
      state: VMStatus::Initializing { request_id: Uuid::new_v4() },
      overlayed_changeset: initial_change_set,
      supplied_data: HashMap::new(),
    }
  }

  fn decode_program(program_str: &str) -> Result<Vec<Instruction>, String> {
    let bytecode = Self::parse_program_to_bytecode(program_str)?;
    let instructions = instruction::decode_bytecode_to_instruction(&bytecode)?;

    Ok(instructions)
  }

  fn execute_next_instruction(&mut self) -> Result<Option<VMEvent>, String> {
    match &self.instructions {
      None => return Err("No instructions found".to_string()),
      Some(instructions) => {
        if self.pc >= instructions.len() {
          self.state = VMStatus::Finished { change_set: self.overlayed_changeset.clone() };
          return Ok(None);
        }

        let instruction = instructions[self.pc].clone();

        // only pay for gas if vm is running,
        // otherwise the gas is already deducted
        match self.state {
          VMStatus::Running => {
            let gas_cost = instruction::get_instruction_gas_cost(&instruction);

            if self.overlayed_changeset.gas_remaining < U256::from(gas_cost) {
              return Err("Out of gas".to_string());
            }

            self.overlayed_changeset.gas_used += U256::from(gas_cost);
            self.overlayed_changeset.gas_remaining -= U256::from(gas_cost);
          }
          _ => {}
        };

        match self.execute_instruction(&instruction) {
          Ok(event) => {
            Ok(event)
          }
          Err(err) => {
            self.overlayed_changeset.status = false;
            self.overlayed_changeset.message = err.clone();
            self.state = VMStatus::Finished {
              change_set: self.overlayed_changeset.clone()
            };
            Err(err)
          }
        }
      }
    }
  }

  pub fn parse_program_to_bytecode(program: &str) -> Result<Vec<u8>, String> {
    let mut bytecode = Vec::new();
    let mut i = 0;

    let program: String = program.chars()
      .filter(|c| !c.is_whitespace())
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

  fn execute_instruction(&mut self, instruction: &Instruction) 
    -> Result<Option<VMEvent>, String> {
      match instruction {
        // stack operation
        Instruction::Push { value } => {
          self.stack.push(*value);
          // println!("[VM] PUSH {}", value);
          self.pc += 1;
          Ok(None)
        }
        Instruction::Pop => {
          if let Some(value) = self.stack.pop() {
            // println!("[VM] POP {}", value);
            self.pc += 1;
            Ok(None)
          } else {
            Err("[VM] POP failed: stack is empty".to_string())
          }
        }
        Instruction::Copy => {
          if let Some(value) = self.stack.last() {
            self.stack.push(*value);
            // println!("[VM] COPY {}", value);
            self.pc += 1;
            Ok(None)
          } else {
            Err("[VM] COPY failed: stack is empty".to_string())
          }
        }

        // arithmetic operations
        Instruction::Add => {
          if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
            let result = a + b;
            self.stack.push(result);
            // println!("[VM] ADD {} + {} = {}", b, a, result);
            self.pc += 1;
            Ok(None)
          } else {
            Err("[VM] ADD failed: insufficient operands on stack".to_string())
          }
        }
        Instruction::Sub => {
          if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
            let result = b - a;
            self.stack.push(result);
            // println!("[VM] SUB {} - {} = {}", b, a, result);
            self.pc += 1;
            Ok(None)
          } else {
            Err("[VM] SUB failed: insufficient operands on stack".to_string())
          }
        }
        Instruction::Mul => {
          if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
            let result = a * b;
            self.stack.push(result);
            // println!("[VM] MUL {} * {} = {}", b, a, result);
            self.pc += 1;
            Ok(None)
          } else {
            Err("[VM] MUL failed: insufficient operands on stack".to_string())
          }
        }
        Instruction::Div => {
          if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
            if a == 0 {
              // println!("[VM] DIV failed: division by zero");
              self.stack.push(U256::from(0 as u64));
              Err("[VM] DIV failed: division by zero".to_string())
            } else {
              let result = b / a;
              self.stack.push(result);
              // println!("[VM] DIV {} / {} = {}", b, a, result);
              self.pc += 1;
              Ok(None)
            }
          } else {
            Err("[VM] DIV failed: insufficient operands on stack".to_string())
          }
        }
        Instruction::Mod => {
          if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
            if a == 0 {
              println!("[VM] MOD failed: division by zero");
              self.stack.push(U256::from(0 as u64));
              Err("[VM] MOD failed: division by zero".to_string())
            } else {
              let result = b % a;
              self.stack.push(result);
              // println!("[VM] MOD {} % {} = {}", b, a, result);
              self.pc += 1;
              Ok(None)
            }
          } else {
            Err("[VM] MOD failed: insufficient operands on stack".to_string())
          }
        }

        // logical/comparison operations
        Instruction::Eq => {
          if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
            let result = if a == b { 1 as u64 } else { 0 as u64 };
            self.stack.push(result.into());
            // println!("[VM] EQ {} == {} = {}", b, a, result);
            self.pc += 1;
            Ok(None)
          } else {
            Err("[VM] EQ failed: insufficient operands on stack".to_string())
          }
        }
        Instruction::Ne => {
          if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
            let result = if a != b { 1 as u64 } else { 0 as u64 };
            self.stack.push(result.into());
            // println!("[VM] NE {} != {} = {}", b, a, result);
            self.pc += 1;
            Ok(None)
          } else {
            Err("[VM] NE failed: insufficient operands on stack".to_string())
          }
        }
        Instruction::Lt => {
          if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
            let result = if b < a { 1 as u64 } else { 0 as u64 };
            self.stack.push(result.into());
            // println!("[VM] LT {} < {} = {}", b, a, result);
            self.pc += 1;
            Ok(None)
          } else {
            Err("[VM] LT failed: insufficient operands on stack".to_string())
          }
        }
        Instruction::Gt => {
          if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
            let result = if b > a { 1 } else { 0 };
            self.stack.push(U256::from(result as u64));
            // println!("[VM] GT {} > {} = {}", b, a, result);
            self.pc += 1;
            Ok(None)
          } else {
            Err("[VM] GT failed: insufficient operands on stack".to_string())
          }
        }
        Instruction::Le => {
          if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
            let result = if b <= a { 1 } else { 0 };
            self.stack.push(U256::from(result as u64));
            // println!("[VM] LE {} <= {} = {}", b, a, result);
            self.pc += 1;
            Ok(None)
          } else {
            Err("[VM] LE failed: insufficient operands on stack".to_string())
          }
        }
        Instruction::Ge => {
          if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
            let result = if b >= a { 1 } else { 0 };
            self.stack.push(U256::from(result as u64));
            // println!("[VM] GE {} >= {} = {}", b, a, result);
            self.pc += 1;
            Ok(None)
          } else {
            Err("[VM] GE failed: insufficient operands on stack".to_string())
          }
        }
        Instruction::And => {
          if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
            let result = a & b;
            self.stack.push(result);
            // println!("[VM] AND {} & {} = {}", b, a, result);
            self.pc += 1;
            Ok(None)
          } else {
            Err("[VM] AND failed: insufficient operands on stack".to_string())
          }
        }
        Instruction::Or => {
          if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
            let result = a | b;
            self.stack.push(result);
            // println!("[VM] OR {} | {} = {}", b, a, result);
            self.pc += 1;
            Ok(None)
          } else {
            Err("[VM] OR failed: insufficient operands on stack".to_string())
          }
        }

        // bitwise operations
        Instruction::BAnd => {
          if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
            let result = a & b;
            self.stack.push(result);
            // println!("[VM] AND {} & {} = {}", b, a, result);
            self.pc += 1;
            Ok(None)
          } else {
            Err("[VM] AND failed: insufficient operands on stack".to_string())
          }
        }
        Instruction::BOr => {
          if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
            let result = a | b;
            self.stack.push(result);
            // println!("[VM] OR {} | {} = {}", b, a, result);
            self.pc += 1;
            Ok(None)
          } else {
            Err("[VM] OR failed: insufficient operands on stack".to_string())
          }
        }
        Instruction::BXor => {
          if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
            let result = a ^ b;
            self.stack.push(result);
            // println!("[VM] XOR {} ^ {} = {}", b, a, result);
            self.pc += 1;
            Ok(None)
          } else {
            Err("[VM] XOR failed: insufficient operands on stack".to_string())
          }
        }
        Instruction::BNot => {
          if let Some(a) = self.stack.pop() {
            let result = !a;
            self.stack.push(result);
            // println!("[VM] NOT !{}", a);
            self.pc += 1;
            Ok(None)
          } else {
            Err("[VM] NOT failed: insufficient operands on stack".to_string())
          }
        }
        Instruction::Shl => {
          if let Some(a) = self.stack.pop() {
            let shift = self.stack.pop().unwrap().as_u32();
            let result = a << shift;
            self.stack.push(result);
            // println!("[VM] SHL {} << {} = {}", a, shift, result);
            self.pc += 1;
            Ok(None)
          } else {
            Err("[VM] SHL failed: insufficient operands on stack".to_string())
          }
        }
        Instruction::Shr => {
          if let Some(a) = self.stack.pop() {
            let shift = self.stack.pop().unwrap().as_u32();
            let result = a >> shift;
            self.stack.push(result);
            // println!("[VM] SHR {} >> {} = {}", a, shift, result);
            self.pc += 1;
            Ok(None)
          } else {
            Err("[VM] SHR failed: insufficient operands on stack".to_string())
          }
        }

        // storage operations
        Instruction::Set => {
          if let Some(key) = self.stack.pop() {
            let value = self.stack.pop().unwrap();
            self.storage.insert(key.to_string(), value);
            // println!("[VM] SET {} = {}", key, value);
            self.pc += 1;
            Ok(None)
          } else {
            Err("[VM] SET failed: insufficient operands on stack".to_string())
          }
        }
        Instruction::Get => {
          let key = self.stack.pop().unwrap().to_string();
          if let Some(value) = self.storage.get(&key) {
            self.stack.push(*value);
            // println!("[VM] GET {} = {}", key, value);
            self.pc += 1;
            Ok(None)
          } else {
            Err("[VM] GET failed: key not found in storage".to_string())
          }
        }

        // control flow operations
        Instruction::Jump => {
          let destination = self.stack.pop().unwrap().as_usize();
          // println!("[VM] JUMP {}", destination);
          self.pc = destination;
          Ok(None)
        }
        Instruction::JumpIf => {
          if let Some(destination) = self.stack.pop() {
            if let Some(condition) = self.stack.pop() {
              if condition != 0 {
                let jump_addr = destination.as_usize();
                // println!("[VM] JUMPIF {}", jump_addr);
                self.pc = jump_addr;
              } else {
                self.pc += 1;
              }
              Ok(None)
            } else {
              Err("[VM] JUMPIF failed: insufficient operands on stack".to_string())
            }
          } else {
            Err("[VM] JUMPIF failed: insufficient operands on stack".to_string())
          }
        }
        Instruction::Halt => {
          let msg_len = self.stack.pop().unwrap().as_usize();
          let mut message = Vec::new();
          for _ in 0..msg_len {
            if let Some(value) = self.stack.pop() {
              message.push(value.as_u8());
            } else {
              return Err("[VM] HALT failed: insufficient operands on stack".to_string());
            }
          }
          let message = String::from_utf8(message);
          // println!("[VM] HALT");
          Err(message.unwrap())
        }

        Instruction::Sha256 { values } => {
          let mut str_buffer = String::new();
          for _ in 0..*values {
            if let Some(value) = self.stack.pop() {
              str_buffer.push_str(&value.to_string());
            } else {
              return Err("[VM] SHA256 failed: insufficient operands on stack".to_string());
            }
          }

          let mut hasher = sha2::Sha256::new();
          hasher.update(str_buffer.as_bytes());
          let hash = hasher.finalize();

          let hash_hex = format!("0x{}", hex::encode(hash));
          let u256_hash = U256::from_str_hex(&hash_hex).unwrap();

          self.stack.push(u256_hash);

          self.pc += 1;
          Ok(None)
        }

        // call/message operations
        Instruction::Call => {
          let payload_len = self.stack.pop().unwrap().as_usize();
          let mut payload = Vec::new();
          for _ in 0..payload_len {
            if let Some(value) = self.stack.pop() {
              payload.push(value.as_u8());
            } else {
              return Err("[VM] CALL failed: insufficient operands on stack".to_string());
            }
          }
          let payload_str = String::from_utf8(payload).unwrap();
          let amount = self.stack.pop().unwrap();
          let gas_limit = self.stack.pop().unwrap();
          let address = self.stack.pop().unwrap().to_string();
          // println!("[VM] CALL {} {} {} {}", address, gas_limit, amount, payload);

          match &self.state {
            VMStatus::ReceivedData { request_id } => {
              match self.supplied_data.get(request_id) {
                Some(data) => {
                  let result_overlay: OverlayedChangeSet = serde_json::from_str(data).unwrap();
                  self.pc += 1;
                  self.state = VMStatus::Running;
                  // println!("[VM] CONTRACT CALL FINISHED: {}", data);
                  self.overlayed_changeset.events
                    .append(&mut result_overlay.events.clone());
                  self.overlayed_changeset.internal_transactions
                    .append(&mut result_overlay.internal_transactions.clone());
                  self.overlayed_changeset.gas_used += result_overlay.gas_used;
                  self.overlayed_changeset.gas_remaining -= result_overlay.gas_used;
                  Ok(None)
                }
                None => {
                  Ok(Some(
                      VMEvent::SpawnVM(
                        Transaction {
                          hash: "hash".to_string(),
                          sender: self.transaction.receiver.clone(),
                          receiver: address.clone().to_string(),
                          amount,
                          gas_limit,
                          gas_price: self.transaction.gas_price,
                          nonce: U256::from(0 as u64),
                          payload: payload_str,
                          signature: "signature".to_string(),
                        })))
                }
              }
            }
            _ => {
              Ok(Some(
                  VMEvent::SpawnVM(
                    Transaction {
                      hash: "hash".to_string(),
                      sender: self.transaction.receiver.clone(),
                      receiver: address.clone().to_string(),
                      amount,
                      gas_limit,
                      gas_price: self.transaction.gas_price,
                      nonce: U256::from(0 as u64),
                      payload: payload_str,
                      signature: "signature".to_string(),
                    })))
            }
          }
        }
        Instruction::Transaction => {
          let amount = self.stack.pop().unwrap();
          let receiver = self.stack.pop().unwrap().to_string();

          println!("[VM] TRANSACTION {} {}", receiver, amount);

          match self.state {
            VMStatus::ReceivedData { request_id } => {
              match self.supplied_data.get(&request_id) {
                None => {
                  let req = jsonrpc::new_request(
                    "get_account",
                    json!([self.transaction.receiver.clone()])
                    );
                  Ok(Some(
                      VMEvent::RequestData(req)
                      ))
                }
                Some(data) => {
                  self.pc += 1;

                  let balance = 
                    jsonrpc::deserialize_response(data).unwrap()
                    .result.unwrap()["balance"].take().as_i64().unwrap();
                  let balance_u256 = U256::from(balance as u64);

                  if balance_u256 < amount {
                    return Err("Insufficient balance for internal_transaction".to_string())
                  } else {
                    self.overlayed_changeset.internal_transactions.push(InternalTransaction {
                      sender: self.transaction.receiver.clone(),
                      receiver: receiver.to_string(),
                      amount
                    });

                    Ok(None)
                  }

                }
              }
            }
            _ => {
              let req = jsonrpc::new_request(
                "get_account",
                json!([self.transaction.receiver.clone()])
                );
              Ok(Some(
                  VMEvent::RequestData(req)
                  ))
            }
          }
        }
        Instruction::Emit => {
          let name = self.stack.pop().unwrap().to_string();
          let topic3 = self.stack.pop().unwrap();
          let topic2 = self.stack.pop().unwrap();
          let topic1 = self.stack.pop().unwrap();

          self.overlayed_changeset.events.push(Event {
            name: name.clone(),
            address: self.transaction.receiver.clone(),
            topics: vec![topic1, topic2, topic3],
          });
          
          self.pc += 1;
          Ok(None)
        }
        Instruction::Return => {
          // println!("[VM] RETURN");
          self.state = VMStatus::Finished {
            change_set: self.overlayed_changeset.clone()
          };
          Ok(None)
        }

        // environment operations
        Instruction::GetBalance => {
          let address = self.stack.pop().unwrap().to_string();
          println!("[VM] GET_BALANCE {}", address);
          self.pc += 1;
          Ok(None)
        }
        Instruction::GetCaller => {
          // println!("[VM] GETCALLER");
          self.pc += 1;
          Ok(None)
        }
        Instruction::GetCallAmount => {
          // println!("[VM] GETCALLVALUE");
          self.stack.push(self.transaction.amount);
          self.pc += 1;
          Ok(None)
        }
        Instruction::GetGasPrice => {
          // println!("[VM] GETGASPRICE");
          self.stack.push(self.transaction.gas_price);
          self.pc += 1;
          Ok(None)
        }
        Instruction::GetBlockNumber => {
          // println!("[VM] GETBLOCKNUMBER");
          self.pc += 1;
          Ok(None)
        }
        Instruction::GetBlockTimestamp => {
          // println!("[VM] GETBLOCKTIMESTAMP");
          self.pc += 1;
          Ok(None)
        }
        Instruction::GetCallPayload => {
          let decoded = hex::decode(&self.transaction.payload).unwrap();
          let mut values: Vec<U256> = Vec::new();
          for chunk in decoded.chunks(32) {
            let mut padded = [0u8; 32];
            padded[..chunk.len()].copy_from_slice(chunk);
            let value = U256::from_be_bytes(padded);
            values.push(value);
          }
          // Push in reverse order to maintain stack order
          for value in values.into_iter().rev() {
            self.stack.push(value);
          }
          self.pc += 1;
          Ok(None)
        }

        Instruction::GetCallPayloadSize => {
          let decoded = hex::decode(&self.transaction.payload).unwrap();
          let size = decoded.chunks(32).count();
          self.stack.push(U256::from(size as u64));
          self.pc += 1;
          Ok(None)
        }
      }
    }
}

mod tests {
  use super::*;

  fn mock_vm() -> VM {
    VM::new(Uuid::new_v4(), Transaction {
      hash: "hash".to_string(),
      sender: "sender".to_string(),
      receiver: "receiver".to_string(),
      amount: U256::from(0 as u64),
      gas_limit: U256::from(0 as u64),
      gas_price: U256::from(0 as u64),
      nonce: U256::from(0 as u64),
      payload: "payload".to_string(),
      signature: "signature".to_string(),
    })
  }

  #[test]
  fn test_parse_program_to_bytecode() {
    let program = "600160005260016000f3";
    let bytecode = VM::parse_program_to_bytecode(program).unwrap();
    assert_eq!(bytecode, vec![0x60, 0x01, 0x60, 0x00, 0x52, 0x60, 0x01, 0x60, 0x00, 0xf3]);
  }

  #[test]
  fn test_invalid_program_to_bytecode() {
    let program = "600160005260016000f";
    let bytecode = VM::parse_program_to_bytecode(program);
    assert_eq!(bytecode, Err("Program length must be even".to_string()));
  }

  #[test]
  fn test_instruction_push() {
    let mut vm = mock_vm();
    let instruction = Instruction::Push { value: U256::from(2 as u64) };
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.stack, vec![U256::from(2 as u64)]);
    assert_eq!(vm.pc, 1);
  }

  #[test]
  fn test_instruction_pop() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(2 as u64));
    let instruction = Instruction::Pop;
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.stack, Vec::<U256>::new());
    assert_eq!(vm.pc, 1);
  }

  #[test]
  fn test_instruction_add() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(2 as u64));
    vm.stack.push(U256::from(3 as u64));
    let instruction = Instruction::Add;
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.stack, vec![U256::from(5 as u64)]);
    assert_eq!(vm.pc, 1);
  }

  #[test]
  fn test_instruction_sub() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(3 as u64));
    vm.stack.push(U256::from(2 as u64));
    let instruction = Instruction::Sub;
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.stack, vec![U256::from(1 as u64)]);
    assert_eq!(vm.pc, 1);
  }

  #[test]
  fn test_instruction_mul() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(2 as u64));
    vm.stack.push(U256::from(3 as u64));
    let instruction = Instruction::Mul;
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.stack, vec![U256::from(6 as u64)]);
    assert_eq!(vm.pc, 1);
  }

  #[test]
  fn test_instruction_div() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(6 as u64));
    vm.stack.push(U256::from(3 as u64));
    let instruction = Instruction::Div;
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.stack, vec![U256::from(2 as u64)]);
    assert_eq!(vm.pc, 1);
  }

  // #[test]
  // fn test_instruction_div_by_zero() {
  //   let mut vm = mock_vm();
  //   vm.stack.push(U256::from(6 as u64));
  //   vm.stack.push(U256::from(0 as u64));
  //   let instruction = Instruction::Div;
  //   let result = vm.execute_instruction(&instruction);
  //   assert_eq!(result, Err("[VM] DIV failed: division by zero".to_string()));
  // }

  #[test]
  fn test_instruction_mod() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(6 as u64));
    vm.stack.push(U256::from(4 as u64));
    let instruction = Instruction::Mod;
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.stack, vec![U256::from(2 as u64)]);
    assert_eq!(vm.pc, 1);
  }

  // #[test]
  // fn test_instruction_mod_by_zero() {
  //   let mut vm = mock_vm();
  //   vm.stack.push(U256::from(6 as u64));
  //   vm.stack.push(U256::from(0 as u64));
  //   let instruction = Instruction::Mod;
  //   let result = vm.execute_instruction(&instruction);
  //   assert_eq!(result, Err("[VM] MOD failed: division by zero".to_string()));
  // }

  #[test]
  fn test_instruction_eq() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(2 as u64));
    vm.stack.push(U256::from(2 as u64));
    let instruction = Instruction::Eq;
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.stack, vec![U256::from(1 as u64)]);
    assert_eq!(vm.pc, 1);
  }

  #[test]
  fn test_instruction_ne() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(2 as u64));
    vm.stack.push(U256::from(3 as u64));
    let instruction = Instruction::Ne;
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.stack, vec![U256::from(1 as u64)]);
    assert_eq!(vm.pc, 1);
  }

  #[test]
  fn test_instruction_lt() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(2 as u64));
    vm.stack.push(U256::from(3 as u64));
    let instruction = Instruction::Lt;
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.stack, vec![U256::from(1 as u64)]);
    assert_eq!(vm.pc, 1);
  }

  #[test]
  fn test_instruction_gt() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(3 as u64));
    vm.stack.push(U256::from(2 as u64));
    let instruction = Instruction::Gt;
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.stack, vec![U256::from(1 as u64)]);
    assert_eq!(vm.pc, 1);
  }

  #[test]
  fn test_instruction_ge() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(3 as u64));
    vm.stack.push(U256::from(2 as u64));
    let instruction = Instruction::Ge;
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.stack, vec![U256::from(1 as u64)]);
    assert_eq!(vm.pc, 1);
  }

  #[test]
  fn test_instruction_le() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(2 as u64));
    vm.stack.push(U256::from(3 as u64));
    let instruction = Instruction::Le;
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.stack, vec![U256::from(1 as u64)]);
    assert_eq!(vm.pc, 1);
  }

  #[test]
  fn test_instruction_band() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(3 as u64));
    vm.stack.push(U256::from(2 as u64));
    let instruction = Instruction::BAnd;
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.stack, vec![U256::from(2 as u64)]);
    assert_eq!(vm.pc, 1);
  }

  #[test]
  fn test_instruction_bor() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(3 as u64));
    vm.stack.push(U256::from(2 as u64));
    let instruction = Instruction::BOr;
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.stack, vec![U256::from(3 as u64)]);
    assert_eq!(vm.pc, 1);
  }

  #[test]
  fn test_instruction_bxor() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(3 as u64));
    vm.stack.push(U256::from(2 as u64));
    let instruction = Instruction::BXor;
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.stack, vec![U256::from(1 as u64)]);
    assert_eq!(vm.pc, 1);
  }

  #[test]
  fn test_instruction_shl() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(1 as u64));
    vm.stack.push(U256::from(3 as u64));
    let instruction = Instruction::Shl;
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.stack, vec![U256::from(6 as u64)]);
    assert_eq!(vm.pc, 1);
  }

  #[test]
  fn test_instruction_shr() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(1 as u64));
    vm.stack.push(U256::from(6 as u64));
    let instruction = Instruction::Shr;
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.stack, vec![U256::from(3 as u64)]);
    assert_eq!(vm.pc, 1);
  }

  #[test]
  fn test_instruction_set() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(2 as u64));
    vm.stack.push(U256::from(35 as u64));
    let instruction = Instruction::Set; 
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.storage, HashMap::from_iter(vec![("35".to_string(), U256::from(2 as u64))]));
    assert_eq!(vm.pc, 1);
  }

  #[test]
  fn test_instruction_jump() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(7 as u64));
    let instruction = Instruction::Jump;
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.pc, 7);
  }

  #[test]
  fn test_instruction_jumpif() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(1 as u64));
    vm.stack.push(U256::from(19 as u64));
    let instruction = Instruction::JumpIf;
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.pc, 19);
  }

  #[test]
  fn test_instruction_jumpif_false() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(0 as u64));
    vm.stack.push(U256::from(19 as u64));
    let instruction = Instruction::JumpIf;
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.pc, 1);
  }

  #[test]
  fn test_instruction_halt() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(97 as u64));
    vm.stack.push(U256::from(98 as u64));
    vm.stack.push(U256::from(2 as u64));
    let instruction = Instruction::Halt;
    let result = vm.execute_instruction(&instruction);
    assert_eq!(result, Err("ba".to_string()));
  }

  #[test]
  fn test_instruction_emit() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(1 as u64));
    vm.stack.push(U256::from(2 as u64));
    vm.stack.push(U256::from(3 as u64));

    vm.stack.push(U256::from(12345 as u64));

    let instruction = Instruction::Emit;
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.overlayed_changeset.events, vec![Event {
      name: "12345".to_string(),
      address: vm.transaction.receiver.clone(),
      topics: vec![
        U256::from(1 as u32),
        U256::from(2 as u32),
        U256::from(3 as u32),
      ],
    }]);
    assert_eq!(vm.pc, 1);
  }

  #[test]
  fn test_instruction_sha256() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(1 as u64));
    vm.stack.push(U256::from(2 as u64));
    vm.stack.push(U256::from(3 as u64));

    let instruction = Instruction::Sha256 { values: 2 };
    vm.execute_instruction(&instruction).unwrap();

    let expected_stack: Vec<U256> = vec![
      (U256::from(1 as u64)),
      (U256::from_str_radix(
        "102499409242696708977622367173532599973197161335174963053596431290206366514027",
        10).unwrap())
    ];

    assert_eq!(vm.stack, expected_stack);
    assert_eq!(vm.pc, 1);
  }

  #[test]
  fn test_get_call_payload_empty() {
    let mut vm = mock_vm();
    vm.transaction.payload = "".to_string();
    vm.execute_instruction(&Instruction::GetCallPayload).unwrap();

    assert_eq!(vm.stack.len(), 0);
    assert_eq!(vm.pc, 1);
  }

  #[test]
  fn test_get_call_payload_single_value() {
    let mut vm = mock_vm();
    vm.transaction.payload = 
    "0000000000000000000000000000000000000000000000000000000000000002"
    .to_string();

    vm.execute_instruction(&Instruction::GetCallPayload).unwrap();

    assert_eq!(vm.stack.len(), 1);
    assert_eq!(vm.stack[0], U256::from(2u64));
    assert_eq!(vm.pc, 1);
  }

  #[test]
  fn test_get_call_payload_multiple_values() {
    let mut vm = mock_vm();
    vm.transaction.payload = 
      "00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002".to_string();
    vm.execute_instruction(&Instruction::GetCallPayload).unwrap();

    assert_eq!(vm.stack.len(), 2);
    assert_eq!(vm.stack.pop().unwrap(), U256::from(1u64));
    assert_eq!(vm.stack.pop().unwrap(), U256::from(2u64));
    assert_eq!(vm.pc, 1);
  }

  #[test]
  fn test_instruction_copy() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(42 as u64));
    let instruction = Instruction::Copy;
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.stack, vec![U256::from(42 as u64), U256::from(42 as u64)]);
    assert_eq!(vm.pc, 1);
  }

  #[test]
  fn test_instruction_copy_empty_stack() {
    let mut vm = mock_vm();
    let instruction = Instruction::Copy;
    let result = vm.execute_instruction(&instruction);
    assert_eq!(result, Err("[VM] COPY failed: stack is empty".to_string()));
  }
}
