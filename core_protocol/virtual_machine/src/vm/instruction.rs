use ethnum::U256;
use super::vm_core::VM;

#[derive(Debug, PartialEq)]
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
  Halt { message: String},

  // call/message operations
  // TODO: unit tests
  Call { address: U256, gas_limit: U256, amount: U256, payload: String },
  Transaction { sender: U256, receiver: U256, amount: U256 },
  Emit { event_name: String, topic1: U256, topic2: U256, topic3: U256, data: Vec<u8> },
  Return,

  // environment operations
  // TODO: unit tests
  GetBalance { address: U256 },
  GetCaller,
  GetCallValue,
  GetGasPrice,
  GetBlockNumber,
  GetBlockTimestamp,
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
        Instruction::Halt { .. } => 2,

        // call/Message Operations
        Instruction::Call { .. } => 10,
        Instruction::Transaction { .. } => 12,
        Instruction::Emit { .. } => 12,
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
        if i + 4 > bytecode.len() {
          return Err("Set instruction expects a key length prefix and key string".to_string());
        }

        let key_len = bytecode[i + 2] as usize;
        if i + 3 + key_len > bytecode.len() {
          return Err("Key length exceeds remaining bytecode length".to_string());
        }

        let key = String::from_utf8(bytecode[i + 3..i + 3 + key_len].to_vec())
          .map_err(|_| "Failed to decode key as UTF-8".to_string())?;

        instructions.push(Instruction::Set { key });
        i += 3 + key_len;
      }
      0x31 => {
        if i + 4 > bytecode.len() {
          return Err("Get instruction expects a key length prefix and key string".to_string());
        }
        let key_len = bytecode[i + 2] as usize;
        if i + 3 + key_len > bytecode.len() {
          return Err("Key length exceeds remaining bytecode length".to_string());
        }
        let key = String::from_utf8(bytecode[i + 3..i + 3 + key_len].to_vec())
          .map_err(|_| "Failed to decode key as UTF-8".to_string())?;
        instructions.push(Instruction::Get { key });
        i += 3 + key_len;
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
        if i + 2 >= bytecode.len() {
          return Err("Halt instruction expects a message length and message data".to_string());
        }

        let message_len = bytecode[i + 1] as usize;
        if i + 2 + message_len > bytecode.len() {
          return Err("Message length exceeds remaining bytecode length".to_string());
        }

        let message = String::from_utf8(bytecode[i + 2..i + 2 + message_len].to_vec())
          .map_err(|_| "Failed to decode event name as UTF-8".to_string())?;
        i += 2 + message_len;

        instructions.push(Instruction::Halt{message});
        i += 1;
      }

      // call/Message Operations
      0x50 => {
        if i + 99 > bytecode.len() {
          return Err("Call instruction expects address (32 bytes), gas limit (32 bytes), amount (32 bytes), and payload length (2 bytes)".to_string());
        }

        // Decode the 32-byte address.
        let address = U256::from_be_bytes(bytecode[i + 1..i + 33].try_into()
          .map_err(|_| "Invalid address".to_string())?);

        // Decode the 32-byte gas limit.
        let gas_limit = U256::from_be_bytes(bytecode[i + 33..i + 65].try_into()
          .map_err(|_| "Invalid gas limit".to_string())?);

        // Decode the 32-byte amount.
        let amount = U256::from_be_bytes(bytecode[i + 65..i + 97].try_into()
          .map_err(|_| "Invalid amount".to_string())?);

        // payload length (2 bytes)
        let payload_len = u16::from_be_bytes([bytecode[i + 97], bytecode[i + 98]]) as usize;

        if i + 99 + payload_len > bytecode.len() {
          return Err("Payload length exceeds remaining bytecode length".to_string());
        }

        // decode the payload as a UTF-8 string if it exists.
        let payload = if payload_len > 0 {
          String::from_utf8(bytecode[i + 99..i + 99 + payload_len].to_vec())
            .map_err(|_| "Failed to decode payload as UTF-8".to_string())?
        } else {
          String::new()
        };

        instructions.push(Instruction::Call { address, gas_limit, amount, payload });

        i += 99 + payload_len;
      },
      0x70 => {
        if i + 3 > bytecode.len() {
          return Err("Emit instruction expects an event name length prefix and event data".to_string());
        }

        let name_len = bytecode[i + 2] as usize;

        if i + 3 + name_len > bytecode.len() {
          return Err("Event name length exceeds remaining bytecode length".to_string());
        }

        let event_name = String::from_utf8(bytecode[i + 3..i + 3 + name_len].to_vec())
          .map_err(|_| "Failed to decode event name as UTF-8".to_string())?;
        i += 3 + name_len;

        if i + 96 > bytecode.len() {
          return Err("Not enough bytes for the 3 topics".to_string());
        }

        // get 3 topics (can be empty => 0x0)
        let topic1 = U256::from_be_bytes(bytecode[i..i + 32].try_into().unwrap());
        let topic2 = U256::from_be_bytes(bytecode[i + 32..i + 64].try_into().unwrap());
        let topic3 = U256::from_be_bytes(bytecode[i + 64..i + 96].try_into().unwrap());
        i += 96;

        // read data len 
        if i + 2 > bytecode.len() {
          return Err("Not enough bytes for data length".to_string());
        }

        let data_len = u16::from_be_bytes([bytecode[i], bytecode[i + 1]]) as usize;
        i += 2;

        let data = if data_len > 0 {
          if i + data_len > bytecode.len() {
            return Err("Data length exceeds remaining bytecode length".to_string());
          }
          bytecode[i..i + data_len].to_vec()
        } else {
          vec![]
        };

        instructions.push(Instruction::Emit { event_name, topic1, topic2, topic3, data }); 

        i += data_len;
      },
      0x51 => {
        if i + 36 >= bytecode.len() {
          return Err("Transaction instruction expects sender, receiver, and amount".to_string());
        }

        let sender = U256::from_be_bytes(bytecode[i + 1..i + 33].try_into().unwrap());
        let receiver = U256::from_be_bytes(bytecode[i + 33..i + 65].try_into().unwrap());
        let amount = U256::from_be_bytes(bytecode[i + 65..i + 97].try_into().unwrap());

        instructions.push(Instruction::Transaction { sender, receiver, amount });

        i += 97;
      }
      0x52 => {
        instructions.push(Instruction::Return);
        i += 1;
      }

      // environment Operations
      0x60 => {
        if i + 32 > bytecode.len() {
          return Err("GetBalance expects a 32-byte (U256) address".to_string());
        }
        let address = U256::from_be_bytes(bytecode[i + 1..i + 33].try_into().unwrap());
        instructions.push(Instruction::GetBalance { address });
        i += 33;
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

mod tests {
  use super::*;

  #[test]
  fn test_decode_push_instruction() {
    let program = VM::parse_program_to_bytecode("
      01
      000000000000000000000000000000000000000000000000000000000000000a
    ").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Push { value: 10u32.into() }]
      );
  }

  #[test]
  fn test_decode_pop_instruction() {
    let program = VM::parse_program_to_bytecode("02").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Pop]
      );
  }

  #[test]
  fn test_decode_add_instruction() {
    let program = VM::parse_program_to_bytecode("03").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Add]
      );
  }

  #[test]
  fn test_decode_sub_instruction() {
    let program = VM::parse_program_to_bytecode("04").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Sub]
      );
  }

  #[test]
  fn test_decode_mul_instruction() {
    let program = VM::parse_program_to_bytecode("05").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Mul]
      );
  }

  #[test]
  fn test_decode_div_instruction() {
    let program = VM::parse_program_to_bytecode("06").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Div]
      );
  }

  #[test]
  fn test_decode_mod_instruction() {
    let program = VM::parse_program_to_bytecode("07").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Mod]
      );
  }

  #[test]
  fn test_decode_eq_instruction() {
    let program = VM::parse_program_to_bytecode("10").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Eq]
      );
  }

  #[test]
  fn test_decode_ne_instruction() {
    let program = VM::parse_program_to_bytecode("11").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Ne]
      );
  }

  #[test]
  fn test_decode_lt_instruction() {
    let program = VM::parse_program_to_bytecode("12").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Lt]
      );
  }

  #[test]
  fn test_decode_gt_instruction() {
    let program = VM::parse_program_to_bytecode("13").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Gt]
      );
  }

  #[test]
  fn test_decode_le_instruction() {
    let program = VM::parse_program_to_bytecode("14").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Le]
      );
  }

  #[test]
  fn test_decode_ge_instruction() {
    let program = VM::parse_program_to_bytecode("15").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Ge]
      );
  }

  #[test]
  fn test_decode_band_instruction() {
    let program = VM::parse_program_to_bytecode("20").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::BAnd]
      );
  }

  #[test]
  fn test_decode_bor_instruction() {
    let program = VM::parse_program_to_bytecode("21").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::BOr]
      );
  }

  #[test]
  fn test_decode_bxor_instruction() {
    let program = VM::parse_program_to_bytecode("22").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::BXor]
      );
  }

  #[test]
  fn test_decode_bnot_instruction() {
    let program = VM::parse_program_to_bytecode("23").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::BNot]
      );
  }

  #[test]
  fn test_decode_shl_instruction() {
    let program = VM::parse_program_to_bytecode("24 02").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Shl { shift: 2 }]
      );
  }

  #[test]
  fn test_decode_shr_instruction() {
    let program = VM::parse_program_to_bytecode("25 03").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Shr { shift: 3 }]
      );
  }

  #[test]
  fn test_decode_set_instruction() {
    let program = VM::parse_program_to_bytecode("30 00 04 6a 6f 68 6e").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Set { key: "john".to_string() }]
      );
  }

  #[test]
  fn test_decode_get_instruction() {
    let program = VM::parse_program_to_bytecode("31 00 02 6a 6f").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Get { key: "jo".to_string() }]
      );
  }

  #[test]
  fn test_decode_jump_instruction() {
    let program = VM::parse_program_to_bytecode("40 00 00 00 0a").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Jump { destination: 10 }]
      );
  }

  #[test]
  fn test_decode_jump_if_instruction() {
    let program = VM::parse_program_to_bytecode("41 00 00 00 0a").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::JumpIf { destination: 10 }]
      );
  }

  #[test]
  fn test_decode_halt_instruction() {
    let program = VM::parse_program_to_bytecode("42 03 66 6f 6f").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Halt { message: "foo".to_string() }]
      );
  }

  #[test]
  fn test_decode_call_instruction() {
    let program = VM::parse_program_to_bytecode("
      50
      4d8bbd4e450f1f604139b50fd0fdbebe662e7e17350fd2b31359d7b49c388a50
      0000000000000000000000000000000000000000000000000000000000000023
      0000000000000000000000000000000000000000000000000000000000000000
      00 00
    ").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Call {
        address: U256::from_str_hex(
          "0x4d8bbd4e450f1f604139b50fd0fdbebe662e7e17350fd2b31359d7b49c388a50"
        ).unwrap(),
        gas_limit: 35u32.into(),
        amount: 0u32.into(),
        payload: "".to_string()
      }]
      );
  }     

  #[test]
  fn test_decode_transaction_instruction() {
    let program = VM::parse_program_to_bytecode("
      51
      4d8bbd4e450f1f604139b50fd0fdbebe662e7e17350fd2b31359d7b49c388a50
      5d8bbd4e450f1f604139b50fd0fdbabc662e7e17350fd2b31359d7b49c388a21
      0000000000000000000000000000000000000000000000000000000000000002
      ").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Transaction {
        sender: U256::from_str_hex(
          "0x4d8bbd4e450f1f604139b50fd0fdbebe662e7e17350fd2b31359d7b49c388a50"
        ).unwrap(),
        receiver: U256::from_str_hex(
          "0x5d8bbd4e450f1f604139b50fd0fdbabc662e7e17350fd2b31359d7b49c388a21"
        ).unwrap(),
        amount: 2u32.into()
      }]
      );
  }

  #[test]
  fn test_decode_emit_instruction() {
    let program = VM::parse_program_to_bytecode("
      70
      00 03 66 6f 6f
      0000000000000000000000000000000000000000000000000000000000000002
      0000000000000000000000000000000000000000000000000000000000000002
      0000000000000000000000000000000000000000000000000000000000000002
      00 00
    ").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Emit {
        event_name: "foo".to_string(),
        topic1: 2u32.into(),
        topic2: 2u32.into(),
        topic3: 2u32.into(),
        data: vec![]
      }]);
  }

  #[test]
  fn test_decode_emit_instruction_with_data() {
    let program = VM::parse_program_to_bytecode("
      70
      00 03 66 6f 6f
      0000000000000000000000000000000000000000000000000000000000000002
      0000000000000000000000000000000000000000000000000000000000000003
      0000000000000000000000000000000000000000000000000000000000000002
      00 02 aa aa
    ").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Emit {
        event_name: "foo".to_string(),
        topic1: 2u32.into(),
        topic2: 3u32.into(),
        topic3: 2u32.into(),
        data: vec![170, 170]
      }]);
  }

  #[test]
  fn test_decode_return_instruction() {
    let program = VM::parse_program_to_bytecode("52").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Return]
      );
  }

  #[test]
  fn test_decode_get_balance_instruction() {
    let program = VM::parse_program_to_bytecode("
      60
      5d8bbd4e450f1f604139b50fd0fdbabc662e7e17350fd2b31359d7b49c388a21
    ").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::GetBalance {
        address: U256::from_str_hex(
         "0x5d8bbd4e450f1f604139b50fd0fdbabc662e7e17350fd2b31359d7b49c388a21"
        ).unwrap()
      }]
      );
  }

  #[test]
  fn test_decode_get_caller_instruction() {
    let program = VM::parse_program_to_bytecode("61").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::GetCaller]
      );
  }

  #[test]
  fn test_decode_get_call_value_instruction() {
    let program = VM::parse_program_to_bytecode("62").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::GetCallValue]
      );
  }

  #[test]
  fn test_decode_get_gas_price_instruction() {
    let program = VM::parse_program_to_bytecode("63").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::GetGasPrice]
      );
  }

  #[test]
  fn test_decode_get_block_number_instruction() {
    let program = VM::parse_program_to_bytecode("64").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::GetBlockNumber]
      );
  }

  #[test]
  fn test_decode_get_block_timestamp_instruction() {
    let program = VM::parse_program_to_bytecode("65").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::GetBlockTimestamp]
      );
  }
}
