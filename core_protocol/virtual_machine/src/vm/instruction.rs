use ethnum::U256;

#[derive(Debug, PartialEq, Clone)]
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
  And,
  Or,

  // bitwise Operations
  BAnd,
  BOr,
  BXor,
  BNot,
  Shl,
  Shr,

  // storage Operations
  Set, 
  Get,

  // control Flow Operations
  Jump,
  JumpIf,
  Halt,
  
  // hash Operations
  Sha256 { values: u8 }, // values parameter indicates the number of U256 values to hash

  // call/message operations
  // TODO: unit tests
  Call,
  Transaction,
  Emit,
  Return,

  // environment operations
  // TODO: unit tests
  GetBalance,
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
    | Instruction::Gt | Instruction::Le | Instruction::Ge
    | Instruction::And | Instruction::Or => 4,

    // bitwise Operations
    Instruction::BAnd | Instruction::BOr | Instruction::BXor | Instruction::BNot => 3,
    Instruction::Shl { .. } | Instruction::Shr { .. } => 4,

    // storage Operations
    Instruction::Set { .. } | Instruction::Get { .. } => 5,

    // control Flow Operations
    Instruction::Jump { .. } | Instruction::JumpIf { .. } => 8,
    Instruction::Halt { .. } => 2,

    // hash Operations
    Instruction::Sha256 { .. } => 10,

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
      0x16 => {
        instructions.push(Instruction::And);
        i += 1;
      }
      0x17 => {
        instructions.push(Instruction::Or);
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
        instructions.push(Instruction::Shl);
        i += 1;
      }
      0x25 => {
        instructions.push(Instruction::Shr);
        i += 1;
      }

      // storage Operations
      0x30 => {
        instructions.push(Instruction::Set);
        i += 1;
      }
      0x31 => {
        instructions.push(Instruction::Get);
        i += 1;
      }

      // control Flow Operations
      0x40 => {
        instructions.push(Instruction::Jump);
        i += 1;
      }
      0x41 => {
        instructions.push(Instruction::JumpIf);
        i += 1;
      }
      0x42 => {
        instructions.push(Instruction::Halt);
        i += 1;
      }
      
      // hash Operations
      0x43 => {
        if i + 1 >= bytecode.len() {
          return Err("Sha256 instruction expects a length".to_string());
        }

        let values = bytecode[i + 32];
        instructions.push(Instruction::Sha256 { values });
        i += 33;
      }

      // call/Message Operations
      0x50 => {
        instructions.push(Instruction::Call);
        i += 1;
      },
      0x70 => {
        instructions.push(Instruction::Emit); 
        i += 1;
      },
      0x51 => {
        instructions.push(Instruction::Transaction);
        i += 1;
      }
      0x52 => {
        instructions.push(Instruction::Return);
        i += 1;
      }

      // environment Operations
      0x60 => {
        instructions.push(Instruction::GetBalance);
        i += 1;
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
  use super::super::vm_core::VM;

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
  fn test_decode_and_instruction() {
    let program = VM::parse_program_to_bytecode("16").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::And]
      );
  }

  #[test]
  fn test_decode_or_instruction() {
    let program = VM::parse_program_to_bytecode("17").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Or]
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
    let program = VM::parse_program_to_bytecode("24").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Shl]
      );
  }

  #[test]
  fn test_decode_shr_instruction() {
    let program = VM::parse_program_to_bytecode("25").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Shr]
      );
  }

  #[test]
  fn test_decode_set_instruction() {
    let program = VM::parse_program_to_bytecode("30").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Set]
      );
  }

  #[test]
  fn test_decode_get_instruction() {
    let program = VM::parse_program_to_bytecode("31").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Get]
      );
  }

  #[test]
  fn test_decode_jump_instruction() {
    let program = VM::parse_program_to_bytecode("40").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Jump]
      );
  }

  #[test]
  fn test_decode_jump_if_instruction() {
    let program = VM::parse_program_to_bytecode("41").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::JumpIf]
      );
  }

  #[test]
  fn test_decode_halt_instruction() {
    let program = VM::parse_program_to_bytecode("42").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Halt]
      );
  }

  #[test]
  fn test_decode_call_instruction() {
    let program = VM::parse_program_to_bytecode("50").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Call]
      );
  }     

  #[test]
  fn test_decode_transaction_instruction() {
    let program = VM::parse_program_to_bytecode("51").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Transaction]
      );
  }

  #[test]
  fn test_decode_emit_instruction() {
    let program = VM::parse_program_to_bytecode("70").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Emit]);
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
    let program = VM::parse_program_to_bytecode("60").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::GetBalance]
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

  #[test]
  fn test_decode_sha256_function() {
    let program = VM::parse_program_to_bytecode("
      43
      0000000000000000000000000000000000000000000000000000000000000002
    ").unwrap();
    assert_eq!(
      decode_bytecode_to_instruction(&program).unwrap(),
      vec![Instruction::Sha256 { values: 2 }]
    );
  }
}
