use ethnum::U256;

#[derive(Debug)]
pub enum Instruction {
  // stack Operations
  Push { value: U256 },
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
  BAnd,
  BOr,
  BXor,
  BNot,
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
  // TODO: unit tests
  Call { address: String, gas_limit: U256, value: U256 },
  Transaction { sender: String, receiver: String, amount: U256 },
  Emit { event_name: String, topic1: U256, topic2: U256, topic3: U256, data: Vec<u8> },
  Return,

  // environment operations
  // TODO: unit tests
  GetBalance { address: String },
  GetCaller,
  GetCallValue,
  GetGasPrice,
  GetBlockNumber,
  GetBlockTimestamp,
  }

  pub fn decode_bytecode_to_instruction(bytecode: &[u8]) -> Result<Vec<Instruction>, String> {
    let mut instructions = Vec::new();
    let mut i = 0;

    while i < bytecode.len() {
      match bytecode[i] {
        // stack Operations
        0x01 => {
          if i + 32 >= bytecode.len() {
            return Err("Push instruction expects a 32-byte (U256) value".to_string());
          }

          let value = U256::from_be_bytes(bytecode[i + 1..i + 33].try_into().unwrap());
          instructions.push(Instruction::Push { value });

          i += 33;
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
          instructions.push(Instruction::BAnd);
          i += 1;
        }
        0x21 => {
          instructions.push(Instruction::BOr);
          i += 1;
        }
        0x22 => {
          instructions.push(Instruction::BXor);
          i += 1;
        }
        0x23 => {
          instructions.push(Instruction::BNot);
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
          if i + 68 >= bytecode.len() {
            return Err("Call instruction expects address, gas limit, and value".to_string());
          }

          let address = String::from_utf8(bytecode[i + 1..i + 5].to_vec()).map_err(|_| "Invalid address".to_string())?;
          let gas_limit = U256::from_be_bytes(bytecode[i + 5..i + 37].try_into().unwrap());
          let value = U256::from_be_bytes(bytecode[i + 37..i + 69].try_into().unwrap());

          instructions.push(Instruction::Call { address, gas_limit, value });

          i += 69;
        },
        0x70 => {
          if i + 2 >= bytecode.len() {
            return Err("Emit instruction expects an event name length prefix and event data".to_string());
          }

          let name_len = bytecode[i + 1] as usize;
          if i + 2 + name_len > bytecode.len() {
            return Err("Event name length exceeds remaining bytecode length".to_string());
          }

          let event_name = String::from_utf8(bytecode[i + 2..i + 2 + name_len].to_vec())
            .map_err(|_| "Failed to decode event name as UTF-8".to_string())?;
          i += 2 + name_len;

          // get 3 topics (can be empty => 0x0)
          let topic1 = U256::from_be_bytes(bytecode[i..i + 32].try_into().unwrap());
          let topic2 = U256::from_be_bytes(bytecode[i + 32..i + 64].try_into().unwrap());
          let topic3 = U256::from_be_bytes(bytecode[i + 64..i + 96].try_into().unwrap());
          i += 96;

          // read data len 
          if i + 32 > bytecode.len() {
            return Err("Not enough bytes for data length".to_string());
          }
          let data_len = U256::from_be_bytes(bytecode[i..i + 32].try_into().unwrap()).as_usize();
          i += 32;
          
          if i + data_len > bytecode.len() {
            return Err("Data length exceeds remaining bytecode length".to_string());
          }
          let data = bytecode[i..i + data_len].to_vec();
          i += data_len;

          instructions.push(Instruction::Emit { event_name, topic1, topic2, topic3, data }); 
      },
      0x51 => {
        if i + 36 >= bytecode.len() {
          return Err("Transaction instruction expects sender, receiver, and amount".to_string());
        }

        let sender = String::from_utf8(bytecode[i + 1..i + 5].to_vec()).map_err(|_| "Invalid sender".to_string())?;
        let receiver = String::from_utf8(bytecode[i + 5..i + 9].to_vec()).map_err(|_| "Invalid receiver".to_string())?;
        let amount = U256::from_be_bytes(bytecode[i + 9..i + 41].try_into().unwrap());
        instructions.push(Instruction::Transaction { sender, receiver, amount });

        i += 41;  
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

pub fn get_instruction_gas_cost(instruction: &Instruction) -> u32 {
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
        Instruction::BAnd | Instruction::BOr | Instruction::BXor | Instruction::BNot => 3,
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

