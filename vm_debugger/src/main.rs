use rustyline::DefaultEditor;
use rustyline::error::ReadlineError;
use std::collections::HashMap;
use std::fs;
use virtual_machine::create_debug_vm;
use virtual_machine::vm::instruction::Instruction;
use virtual_machine::vm::vm_core::{VM, VMStateMachine, VMStatus, Transaction, OverlayedChangeSet};
use virtual_machine::vm::instruction::decode_bytecode_to_instruction;
use uuid::Uuid;
use ethnum::U256;

struct VMDebugger {
    vm: VM,
    history: Vec<VM>,  // For state rollback
    breakpoints: Vec<usize>,  // Program counter breakpoints
}

impl VMDebugger {
    pub fn new() -> Self {
        // Create a minimal VM with just the essentials
        let vm = VM {
            id: Uuid::new_v4(),
            transaction: Transaction {
                hash: String::new(),
                sender: String::new(),
                receiver: String::new(),
                amount: U256::from(0 as u8),
                gas_limit: U256::from(u64::MAX), // Set high gas limit for debugging
                gas_price: U256::from(0 as u8),
                nonce: U256::from(0 as u8),
                payload: "0123456789abcdef".to_string(), // Mock payload for debugging
                signature: String::new(),
            },
            storage: HashMap::new(),
            stack: Vec::new(),
            memory: Vec::new(),
            pc: 0,
            instructions: None,
            state: VMStatus::Running,
            overlayed_changeset: OverlayedChangeSet {
                status: true,
                message: String::new(),
                vm_id: Uuid::new_v4(),
                events: Vec::new(),
                internal_transactions: Vec::new(),
                gas_limit: U256::from(u64::MAX),
                gas_used: U256::from(0 as u8),
                gas_remaining: U256::from(u64::MAX),
            },
            supplied_data: HashMap::new(),
        };
        
        VMDebugger {
            vm,
            history: Vec::new(),
            breakpoints: Vec::new(),
        }
    }

    // Take a snapshot of the VM state for undo
    pub fn save_state(&mut self) {
        self.history.push(self.vm.clone());
    }

    // Step execution
    pub fn step(&mut self) -> VMStatus {
        self.save_state();
        VMStateMachine::poll(&mut self.vm)
    }

    // Undo last operation
    pub fn undo(&mut self) -> bool {
        if let Some(previous_state) = self.history.pop() {
            self.vm = previous_state;
            true
        } else {
            false
        }
    }

    // Add a breakpoint
    pub fn add_breakpoint(&mut self, pc: usize) {
        if !self.breakpoints.contains(&pc) {
            self.breakpoints.push(pc);
            self.breakpoints.sort(); // Keep sorted for easier viewing
        }
    }

    // Remove a breakpoint
    pub fn remove_breakpoint(&mut self, pc: usize) {
        self.breakpoints.retain(|&x| x != pc);
    }

    // Run until hitting a breakpoint
    pub fn run_until_breakpoint(&mut self) -> VMStatus {
        let mut status = VMStatus::Running;
        
        loop { 
            if self.breakpoints.contains(&self.vm.pc) {
                println!("Breakpoint hit at PC: {}", self.vm.pc);
                break;
            }
            self.save_state();
            status = VMStateMachine::poll(&mut self.vm);
            
            // Check if we should stop
            match status {
                VMStatus::Finished { .. } => break,
                VMStatus::Running => {}, // continue
                _ => break, // Other states like Pending or ReceivedData should break
            }
        }
        
        status
    }

    // Get the current instruction
    pub fn current_instruction(&self) -> Option<&Instruction> {
        self.vm.instructions.as_ref()
            .and_then(|instructions| instructions.get(self.vm.pc))
    }

    // Load bytecode from a file
    pub fn load_bytecode(&mut self, filename: &str) -> Result<(), String> {
        match fs::read_to_string(filename) {
            Ok(hex_string) => {
                // Reset state
                self.history.clear();
                self.breakpoints.clear();
                
                // Create a new VM instance
                let mut vm = VM {
                    id: Uuid::new_v4(),
                    transaction: Transaction {
                        hash: String::new(),
                        sender: String::new(),
                        receiver: String::new(),
                        amount: U256::from(0 as u8),
                        gas_limit: U256::from(u64::MAX),
                        gas_price: U256::from(0 as u8),
                        nonce: U256::from(0 as u8),
                        payload: "0123456789abcdef".to_string(), // Mock payload for debugging
                        signature: String::new(),
                    },
                    storage: HashMap::new(),
                    stack: Vec::new(),
                    memory: Vec::new(),
                    pc: 0,
                    instructions: None,
                    state: VMStatus::Running,
                    overlayed_changeset: OverlayedChangeSet {
                        status: true,
                        message: String::new(),
                        vm_id: Uuid::new_v4(),
                        events: Vec::new(),
                        internal_transactions: Vec::new(),
                        gas_limit: U256::from(u64::MAX),
                        gas_used: U256::from(0 as u8),
                        gas_remaining: U256::from(u64::MAX),
                    },
                    supplied_data: HashMap::new(),
                };
                
                // Parse and load the bytecode directly
                match VM::parse_program_to_bytecode(&hex_string) {
                    Ok(bytecode) => {
                        match decode_bytecode_to_instruction(&bytecode) {
                            Ok(instructions) => {
                                vm.instructions = Some(instructions);
                                self.vm = vm;
                                Ok(())
                            },
                            Err(e) => Err(format!("Failed to parse bytecode: {}", e))
                        }
                    },
                    Err(e) => Err(format!("Failed to parse hex string: {}", e))
                }
            },
            Err(e) => Err(format!("Failed to read file: {}", e))
        }
    }
}

fn print_help() {
    println!("\n=== Available Commands ===");
    println!("  step (s)            - Execute next instruction");
    println!("  continue (c)        - Run until next breakpoint or completion");
    println!("  undo (u)            - Undo last operation");
    println!("  breakpoint (b) <pc> - Set breakpoint at program counter");
    println!("  delbreak (db) <pc>  - Remove breakpoint");
    println!("  breaks              - List all breakpoints");
    println!("  stack               - Show current stack contents");
    println!("  memory              - Show memory contents");
    println!("  storage             - Show storage contents");
    println!("  pc                  - Show current program counter");
    println!("  instruction  (i)    - Show current instruction");
    println!("  instructions (is)   - Show all loaded instructions");
    println!("  load <file>         - Load bytecode from file");
    println!("  status              - Show VM status");
    println!("  reset               - Reset VM to initial state");
    println!("  help (h)            - Show this help message");
    println!("  quit (q)            - Exit debugger");
    println!(""); // Empty line for better readability
}

fn main() {
    println!("Virtual Machine Debugger");
    println!("Type 'help' for available commands");
    
    let mut debugger = VMDebugger::new();
    let mut rl = DefaultEditor::new().unwrap();
    
    loop {
        match rl.readline("debug> ") {
            Ok(line) => {
                let args: Vec<&str> = line.trim().split_whitespace().collect();
                if args.is_empty() {
                    continue;
                }
                
                match args[0] {
                    "step" | "s" => {
                        let status = debugger.step();
                        println!("\n=== Step Execution ===");
                        println!("Status: {:?}", status);
                        println!("PC: {}", debugger.vm.pc);
                        
                        if let Some(instruction) = debugger.current_instruction() {
                            println!("\nInstruction: {:?}", instruction);
                            println!("\nStack:");
                            if debugger.vm.stack.is_empty() {
                                println!("  <empty>");
                            } else {
                                for (i, value) in debugger.vm.stack.iter().enumerate() {
                                    println!("  {}: {:?}", i, value);
                                }
                            }
                        } else {
                            println!("\nNo instruction at current PC");
                        }
                        println!(""); // Empty line for better readability
                    },
                    "continue" | "c" => {
                        let status = debugger.run_until_breakpoint();
                        println!("\n=== Execution Paused ===");
                        println!("Status: {:?}", status);
                        println!("PC: {}", debugger.vm.pc);
                        println!("\nStack:");
                        if debugger.vm.stack.is_empty() {
                            println!("  <empty>");
                        } else {
                            for (i, value) in debugger.vm.stack.iter().enumerate() {
                                println!("  {}: {:?}", i, value);
                            }
                        }
                        println!(""); // Empty line for better readability
                    },
                    "undo" | "u" => {
                        if debugger.undo() {
                            println!("\n=== Reverted to Previous State ===");
                            println!("PC: {}", debugger.vm.pc);
                            println!("\nStack:");
                            if debugger.vm.stack.is_empty() {
                                println!("  <empty>");
                            } else {
                                for (i, value) in debugger.vm.stack.iter().enumerate() {
                                    println!("  {}: {:?}", i, value);
                                }
                            }
                        } else {
                            println!("\n⚠️ No previous state available");
                        }
                        println!(""); // Empty line for better readability
                    },
                    "breakpoint" | "b" => {
                        if args.len() < 2 {
                            println!("\nUsage: breakpoint <pc>");
                        } else if let Ok(pc) = args[1].parse::<usize>() {
                            debugger.add_breakpoint(pc);
                            println!("\n✅ Breakpoint set at PC: {}", pc);
                        } else {
                            println!("\n⚠️ Invalid PC value");
                        }
                        println!(""); // Empty line for better readability
                    },
                    "delbreak" | "db" => {
                        if args.len() < 2 {
                            println!("\nUsage: delbreak <pc>");
                        } else if let Ok(pc) = args[1].parse::<usize>() {
                            debugger.remove_breakpoint(pc);
                            println!("\n✅ Breakpoint removed at PC: {}", pc);
                        } else {
                            println!("\n⚠️ Invalid PC value");
                        }
                        println!(""); // Empty line for better readability
                    },
                    "breaks" => {
                        println!("\n=== Breakpoints ===");
                        if debugger.breakpoints.is_empty() {
                            println!("No breakpoints set");
                        } else {
                            for (i, pc) in debugger.breakpoints.iter().enumerate() {
                                println!("  {}: PC {}", i + 1, pc);
                            }
                        }
                        println!(""); // Empty line for better readability
                    },
                    "stack" => {
                        println!("\n=== Stack ===");
                        if debugger.vm.stack.is_empty() {
                            println!("  <empty>");
                        } else {
                            for (i, value) in debugger.vm.stack.iter().enumerate() {
                                println!("  {}: {:?}", i, value);
                            }
                        }
                        println!(""); // Empty line for better readability
                    },
                    "memory" => {
                        println!("\n=== Memory ===");
                        if debugger.vm.memory.is_empty() {
                            println!("  <empty>");
                        } else {
                            // Display memory in chunks of 16 bytes
                            for (i, chunk) in debugger.vm.memory.chunks(16).enumerate() {
                                let offset = i * 16;
                                print!("  0x{:04x}: ", offset);
                                for byte in chunk {
                                    print!("{:02x} ", byte);
                                }
                                println!("");
                            }
                        }
                        println!(""); // Empty line for better readability
                    },
                    "storage" => {
                        println!("\n=== Storage ===");
                        if debugger.vm.storage.is_empty() {
                            println!("  <empty>");
                        } else {
                            for (key, value) in &debugger.vm.storage {
                                println!("  {} => {:?}", key, value);
                            }
                        }
                        println!(""); // Empty line for better readability
                    },
                    "pc" => {
                        println!("\n=== Program Counter ===");
                        println!("PC: {}", debugger.vm.pc);
                        println!(""); // Empty line for better readability
                    },
                    "instruction" | "i" => {
                        println!("\n=== Current Instruction ===");
                        if let Some(instruction) = debugger.current_instruction() {
                            println!("PC {}: {:?}", debugger.vm.pc, instruction);
                        } else {
                            println!("No instruction at current PC: {}", debugger.vm.pc);
                        }
                        println!(""); // Empty line for better readability
                    },
                    "instructions" | "is" => {
                        println!("\n=== Program Instructions ===");
                        if let Some(instructions) = &debugger.vm.instructions {
                            if instructions.is_empty() {
                                println!("  <no instructions loaded>");
                            } else {
                                for (i, instruction) in instructions.iter().enumerate() {
                                    let current = if i == debugger.vm.pc { "→ " } else { "  " };
                                    println!("{}[{:04}] {:?}", current, i, instruction);
                                }
                            }
                        } else {
                            println!("  <no program loaded>");
                        }
                        println!(""); // Empty line for better readability
                    },
                    "load" => {
                        if args.len() < 2 {
                            println!("\nUsage: load <filename>");
                        } else {
                            println!("\n=== Loading Bytecode ===");
                            println!("File: {}", args[1]);
                            match debugger.load_bytecode(args[1]) {
                                Ok(_) => println!("✅ Bytecode loaded successfully"),
                                Err(e) => println!("⚠️ Error: {}", e),
                            }
                        }
                        println!(""); // Empty line for better readability
                    },
                    "status" => {
                        println!("\n=== VM Status ===");
                        println!("VM ID: {}", debugger.vm.id);
                        println!("PC: {}", debugger.vm.pc);
                        println!("Stack size: {}", debugger.vm.stack.len());
                        println!("Memory size: {} bytes", debugger.vm.memory.len());
                        println!("Storage entries: {}", debugger.vm.storage.len());
                        println!("Instructions: {}", 
                            if let Some(instructions) = &debugger.vm.instructions {
                                format!("{} instructions", instructions.len())
                            } else {
                                "None".to_string()
                            });
                        println!(""); // Empty line for better readability
                    },
                    "reset" => {
                        debugger = VMDebugger::new();
                        println!("\n✅ VM reset to initial state");
                        println!(""); // Empty line for better readability
                    },
                    "help" | "h" => {
                        print_help();
                    },
                    "quit" | "q" | "exit" => {
                        println!("\nExiting debugger. Goodbye!");
                        break;
                    },
                    _ => {
                        println!("\n⚠️ Unknown command. Type 'help' for available commands.");
                        println!(""); // Empty line for better readability
                    }
                }
                let _ = rl.add_history_entry(line.as_str());
            },
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => {
                break;
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    
    println!("Exiting debugger");
}
