use uuid::Uuid;
use ethnum::U256;
use std::collections::HashMap;
use serde::Serialize;
use serde_json::json;
use std::sync::mpsc::{Receiver, RecvTimeoutError};
use super::super::{jsonrpc, jsonrpc::JsonRpcResponse};
use super::event::Event;
use super::instruction;
use super::instruction::Instruction;
use super::ocaml_callback::{OcamlCallback, MockOcamlCallback};

#[derive(Serialize, Debug)]
pub struct OverlayedChangeSet {
  pub status: bool,
  pub message: String,
  pub vm_id: Uuid,
  pub parent_vm: Option<Uuid>,
  pub events: Vec<Event>,
  pub internal_transactions: Vec<InternalTransaction>,
}

#[derive(Debug, Serialize, Clone)]
pub struct InternalTransaction {
  pub sender: String,
  pub receiver: String,
  pub amount: U256,
}

#[derive(Debug, Clone)]
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

pub struct VM {
  pub id: Uuid,
  pub rx: Receiver<String>,
  pub transaction: Transaction,
  pub storage: HashMap<String, U256>,
  pub stack: Vec<U256>,
  pub memory: Vec<u8>,
  pub pc: usize,
  pub events: Vec<Event>,
  pub internal_transactions: Vec<InternalTransaction>,
  ocaml_callback: Box<dyn OcamlCallback>,
}

// TODO: 
// - [ ] nested contract calls
//  - [ ] create the transaction
//  - [ ] spawn_vm
//  - [ ] append and propagate result
// - [ ] pointers to handle string data
//  - [ ] handle strings
//  - [ ] handle dictionaries
// - [x] emit events
// - [x] internal transactions
//
//
// examples:
//
// 01                                                                // push
// 000000000000000000000000000000000000000000000000000000000000000a  // 
// 01                                                                // push
// 0000000000000000000000000000000000000000000000000000000000000014  // push
// 03                                                                // sum
//
// 70                                                                // emit
// 08                                                                // event_name length (8 bytes)
// 5472616e73666572                                                  // UTF-8 encoded "Transfer"
// 0100000000000000000000000000000000000000000000000000000000000000  // Topic 1
// 0200000000000000000000000000000000000000000000000000000000000000  // Topic 2
// 0300000000000000000000000000000000000000000000000000000000000000  // Topic 3
// 0000000000000000000000000000000000000000000000000000000000000020  // data lenght (32 bytes)
// 000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F  // Event Data
// 

impl VM {
  pub fn new(id: Uuid, rx: Receiver<String>, transaction: Transaction, ocaml_callback: Box<dyn OcamlCallback>) -> Self {
    VM {
      id,
      rx,
      transaction,
      storage: HashMap::new(),
      stack: vec!(),
      memory: vec!(),
      pc: 0,
      events: vec!(),
      internal_transactions: vec!(),
      ocaml_callback,
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

      let bytecode = Self::parse_program_to_bytecode(program.as_ref().unwrap());
      if let Err(err) = &bytecode {
        (false, format!("Failed to parse program to bytecode: {}", err));
      }

      let instructions = instruction::decode_bytecode_to_instruction(bytecode.as_ref().unwrap());
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
      events: self.events.clone(),
      internal_transactions: self.internal_transactions.clone()
    };

    vec![change_set]
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

  fn execute_program(&mut self, program: &[Instruction], gas_limit: U256) -> Result<(), String> {
    let mut gas_limit = gas_limit;

    while self.pc < program.len() {
      let opcode = &program[self.pc];
      let gas_cost = instruction::get_instruction_gas_cost(opcode);

      if gas_limit < U256::from(gas_cost) {
        return Err("Out of gas".to_string());
      }

      gas_limit = gas_limit - U256::from(gas_cost);
      self.execute_instruction(opcode)?;
    }
    Ok(())
  }

  fn execute_instruction(&mut self, instruction: &Instruction) -> Result<(), String> {
    match instruction {
      // stack operation
      Instruction::Push { value } => {
        self.stack.push(*value);
        println!("[VM] PUSH {}", value);
        self.pc += 1;
        Ok(())
      }
      Instruction::Pop => {
        if let Some(value) = self.stack.pop() {
          println!("[VM] POP {}", value);
          self.pc += 1;
          Ok(())
        } else {
          Err("[VM] POP failed: stack is empty".to_string())
        }
      }

      // arithmetic operations
      Instruction::Add => {
        if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
          let result = a + b;
          self.stack.push(result);
          println!("[VM] ADD {} + {} = {}", b, a, result);
          self.pc += 1;
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
          self.pc += 1;
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
          self.pc += 1;
          Ok(())
        } else {
          Err("[VM] MUL failed: insufficient operands on stack".to_string())
        }
      }
      Instruction::Div => {
        if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
          if a == 0 {
            println!("[VM] DIV failed: division by zero");
            self.stack.push(U256::from(0 as u64));
            Err("[VM] DIV failed: division by zero".to_string())
          } else {
            let result = b / a;
            self.stack.push(result);
            println!("[VM] DIV {} / {} = {}", b, a, result);
            self.pc += 1;
            Ok(())
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
            println!("[VM] MOD {} % {} = {}", b, a, result);
            self.pc += 1;
            Ok(())
          }
        } else {
          Err("[VM] MOD failed: insufficient operands on stack".to_string())
        }
      }

      // logical/comparison operations
      // TODO: AND, OR, NOT, XOR
      Instruction::Eq => {
        if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
          let result = if a == b { 1 as u64 } else { 0 as u64 };
          self.stack.push(result.into());
          println!("[VM] EQ {} == {} = {}", b, a, result);
          self.pc += 1;
          Ok(())
        } else {
          Err("[VM] EQ failed: insufficient operands on stack".to_string())
        }
      }
      Instruction::Ne => {
        if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
          let result = if a != b { 1 as u64 } else { 0 as u64 };
          self.stack.push(result.into());
          println!("[VM] NE {} != {} = {}", b, a, result);
          self.pc += 1;
          Ok(())
        } else {
          Err("[VM] NE failed: insufficient operands on stack".to_string())
        }
      }
      Instruction::Lt => {
        if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
          let result = if b < a { 1 as u64 } else { 0 as u64 };
          self.stack.push(result.into());
          println!("[VM] LT {} < {} = {}", b, a, result);
          self.pc += 1;
          Ok(())
        } else {
          Err("[VM] LT failed: insufficient operands on stack".to_string())
        }
      }
      Instruction::Gt => {
        if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
          let result = if b > a { 1 } else { 0 };
          self.stack.push(U256::from(result as u64));
          println!("[VM] GT {} > {} = {}", b, a, result);
          self.pc += 1;
          Ok(())
        } else {
          Err("[VM] GT failed: insufficient operands on stack".to_string())
        }
      }
      Instruction::Le => {
        if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
          let result = if b <= a { 1 } else { 0 };
          self.stack.push(U256::from(result as u64));
          println!("[VM] LE {} <= {} = {}", b, a, result);
          self.pc += 1;
          Ok(())
        } else {
          Err("[VM] LE failed: insufficient operands on stack".to_string())
        }
      }
      Instruction::Ge => {
        if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
          let result = if b >= a { 1 } else { 0 };
          self.stack.push(U256::from(result as u64));
          println!("[VM] GE {} >= {} = {}", b, a, result);
          self.pc += 1;
          Ok(())
        } else {
          Err("[VM] GE failed: insufficient operands on stack".to_string())
        }
      }

      // bitwise operations
      Instruction::BAnd => {
        if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
          let result = a & b;
          self.stack.push(result);
          println!("[VM] AND {} & {} = {}", b, a, result);
          self.pc += 1;
          Ok(())
        } else {
          Err("[VM] AND failed: insufficient operands on stack".to_string())
        }
      }
      Instruction::BOr => {
        if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
          let result = a | b;
          self.stack.push(result);
          println!("[VM] OR {} | {} = {}", b, a, result);
          self.pc += 1;
          Ok(())
        } else {
          Err("[VM] OR failed: insufficient operands on stack".to_string())
        }
      }
      Instruction::BXor => {
        if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
          let result = a ^ b;
          self.stack.push(result);
          println!("[VM] XOR {} ^ {} = {}", b, a, result);
          self.pc += 1;
          Ok(())
        } else {
          Err("[VM] XOR failed: insufficient operands on stack".to_string())
        }
      }
      Instruction::BNot => {
        if let Some(a) = self.stack.pop() {
          let result = !a;
          self.stack.push(result);
          println!("[VM] NOT !{}", a);
          self.pc += 1;
          Ok(())
        } else {
          Err("[VM] NOT failed: insufficient operands on stack".to_string())
        }
      }
      Instruction::Shl { shift } => {
        if let Some(a) = self.stack.pop() {
          let result = a << shift;
          self.stack.push(result);
          println!("[VM] SHL {} << {} = {}", a, shift, result);
          self.pc += 1;
          Ok(())
        } else {
          Err("[VM] SHL failed: insufficient operands on stack".to_string())
        }
      }
      Instruction::Shr { shift } => {
        if let Some(a) = self.stack.pop() {
          let result = a >> shift;
          self.stack.push(result);
          println!("[VM] SHR {} >> {} = {}", a, shift, result);
          self.pc += 1;
          Ok(())
        } else {
          Err("[VM] SHR failed: insufficient operands on stack".to_string())
        }
      }

      // storage operations
      Instruction::Set { key } => {
        if let Some(value) = self.stack.pop() {
          self.storage.insert(key.clone(), value);
          println!("[VM] SET {} = {}", key, value);
          self.pc += 1;
          Ok(())
        } else {
          Err("[VM] SET failed: insufficient operands on stack".to_string())
        }
      }
      Instruction::Get { key } => {
        if let Some(value) = self.storage.get(key) {
          self.stack.push(*value);
          println!("[VM] GET {} = {}", key, value);
          self.pc += 1;
          Ok(())
        } else {
          Err("[VM] GET failed: key not found in storage".to_string())
        }
      }
      
      // control flow operations
      Instruction::Jump { destination } => {
        println!("[VM] JUMP {}", destination);
        self.pc = *destination;
        Ok(())
      }
      Instruction::JumpIf { destination } => {
        if let Some(condition) = self.stack.pop() {
          if condition != 0 {
            println!("[VM] JUMPIF {}", destination);
            self.pc = *destination;
          } else {
            self.pc += 1;
          }
          Ok(())
        } else {
          Err("[VM] JUMPIF failed: insufficient operands on stack".to_string())
        }
      }
      Instruction::Halt { message } => {
        println!("[VM] HALT");
        Err(message.to_string())
      }
      
      // call/message operations
      Instruction::Call { address, gas_limit, amount, payload } => {
        println!("[VM] CALL {} {} {} {}", address, gas_limit, amount, payload);
        self.pc += 1;
        Ok(())
      }
      Instruction::Transaction { sender, receiver, amount } => {
        println!("[VM] TRANSACTION {} {} {}", sender, receiver, amount);
        let tx = self.make_transaction(*receiver, *amount)?;
        self.internal_transactions.push(tx);
        self.pc += 1;
        Ok(())
      }
      Instruction::Emit { event_name, data, topic1, topic2, topic3 } => {
        println!("[VM] EMIT: \n
                 name: {}\n
                 topic1: {}\n
                 topic2\n {}\n
                 topic3:{}\n
                 data: {:?}",
                 event_name, topic1, topic2, topic3, data);

        self.events.push(Event {
          address: self.transaction.receiver.clone(),
          topics: vec![*topic1, *topic2, *topic3],
          data: data.clone(),
        });
        self.pc += 1;
        Ok(())
      }
      Instruction::Return => {
        println!("[VM] RETURN");
        Ok(())
      }

      // environment operations
      Instruction::GetBalance { address } => {
        println!("[VM] GETBALANCE {}", address);
        self.pc += 1;
        Ok(())
      }
      Instruction::GetCaller => {
        println!("[VM] GETCALLER");
        self.pc += 1;
        Ok(())
      }
      Instruction::GetCallValue => {
        println!("[VM] GETCALLVALUE");
        self.stack.push(self.transaction.amount);
        self.pc += 1;
        Ok(())
      }
      Instruction::GetGasPrice => {
        println!("[VM] GETGASPRICE");
        self.stack.push(self.transaction.gas_price);
        self.pc += 1;
        Ok(())
      }
      Instruction::GetBlockNumber => {
        println!("[VM] GETBLOCKNUMBER");
        self.pc += 1;
        Ok(())
      }
      Instruction::GetBlockTimestamp => {
        println!("[VM] GETBLOCKTIMESTAMP");
        self.pc += 1;
        Ok(())
      }
    }
  }

  // returns a internal transaction, if the VM returns an error to peer 
  // we can discard the transaction, otherwise the node executes it
  fn make_transaction(&self, receiver: U256, amount: U256) -> Result<InternalTransaction, String> {
    let sender = self.transaction.receiver.clone();

    let req = jsonrpc::serialize_request(
      "get_account",
      json!([sender])
      );

    let res = self.json_rpc(req).unwrap();
    let balance = res.result.unwrap()["balance"].take();
    let balance_u256 = U256::from_str_radix(balance.as_str().unwrap(), 10).unwrap();

    if balance_u256 < amount {
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

    self.ocaml_callback.call(&self.id.to_string(), &json);

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
}

mod tests {
  use super::*;

  fn mock_vm() -> VM {
    let channel = std::sync::mpsc::channel();
    let ocaml_callback = Box::new(MockOcamlCallback);
       
    VM::new(Uuid::new_v4(), channel.1, Transaction {
      hash: "hash".to_string(),
      sender: "sender".to_string(),
      receiver: "receiver".to_string(),
      amount: U256::from(0 as u64),
      gas_limit: U256::from(0 as u64),
      gas_price: U256::from(0 as u64),
      nonce: U256::from(0 as u64),
      payload: "payload".to_string(),
      signature: "signature".to_string(),
    }, ocaml_callback
  )}

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

  #[test]
  fn test_instruction_div_by_zero() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(6 as u64));
    vm.stack.push(U256::from(0 as u64));
    let instruction = Instruction::Div;
    let result = vm.execute_instruction(&instruction);
    assert_eq!(result, Err("[VM] DIV failed: division by zero".to_string()));
  }

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

  #[test]
  fn test_instruction_mod_by_zero() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(6 as u64));
    vm.stack.push(U256::from(0 as u64));
    let instruction = Instruction::Mod;
    let result = vm.execute_instruction(&instruction);
    assert_eq!(result, Err("[VM] MOD failed: division by zero".to_string()));
  }

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

  // #[test]
  // fn test_instruction_bnot() {
  //   let mut vm = mock_vm();
  //   vm.stack.push(U256::from(3 as u64));
  //   let instruction = Instruction::BNot;
  //   vm.execute_instruction(&instruction).unwrap();
  //   assert_eq!(vm.stack, vec![U256::from(0 as u64)]);
  //   assert_eq!(vm.pc, 1);
  // }

  #[test]
  fn test_instruction_shl() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(3 as u64));
    let instruction = Instruction::Shl { shift: 1 };
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.stack, vec![U256::from(6 as u64)]);
    assert_eq!(vm.pc, 1);
  }

  #[test]
  fn test_instruction_shr() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(6 as u64));
    let instruction = Instruction::Shr { shift: 1 };
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.stack, vec![U256::from(3 as u64)]);
    assert_eq!(vm.pc, 1);
  }

  #[test]
  fn test_instruction_set() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(2 as u64));
    let instruction = Instruction::Set { key : "key".to_string() }; 
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.storage, HashMap::from_iter(vec![("key".to_string(), U256::from(2 as u64))]));
    assert_eq!(vm.pc, 1);
  }

  #[test]
  fn test_instruction_jump() {
    let mut vm = mock_vm();
    let instruction = Instruction::Jump { destination: 7 };
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.pc, 7);
  }

  #[test]
  fn test_instruction_jumpif() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(1 as u64));
    let instruction = Instruction::JumpIf { destination: 7 };
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.pc, 7);
  }

  #[test]
  fn test_instruction_jumpif_false() {
    let mut vm = mock_vm();
    vm.stack.push(U256::from(0 as u64));
    let instruction = Instruction::JumpIf { destination: 7 };
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.pc, 1);
  }

  #[test]
  fn test_instruction_halt() {
    let mut vm = mock_vm();
    let instruction = Instruction::Halt { message: "custom_error_message".to_string() };
    let result = vm.execute_instruction(&instruction);
    assert_eq!(result, Err("custom_error_message".to_string()));
  }


  #[test]
  fn test_instruction_emit() {
    let mut vm = mock_vm();
    let instruction = Instruction::Emit { 
      event_name: "event".to_string(),
      data: "data".to_string().into(),
      topic1: U256::from(1 as u32),
      topic2: U256::from(2 as u32),
      topic3: U256::from(3 as u32),
    };
    vm.execute_instruction(&instruction).unwrap();
    assert_eq!(vm.events, vec![Event {
      address: vm.transaction.receiver.clone(),
      topics: vec![
        U256::from(1 as u32),
        U256::from(2 as u32),
        U256::from(3 as u32),
      ],
      data: "data".to_string().into(),
    }]);
    assert_eq!(vm.pc, 1);
  }
}
