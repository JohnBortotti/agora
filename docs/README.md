# Agora Documentation

## Setup Instructions

### Core Protocol

1. Database setup:
   ```bash
   cd /core_protocol/database
   cargo build --release
   ```

2. Virtual Machine setup:
   ```bash
   cd /core_protocol/virtual_machine
   cargo build --release
   ```

3. Start the Core Protocol containers:
   ```bash
   cd /core_protocol/
   docker compose up
   ```

### Node Explorer

1. Start the Explorer container:
   ```bash
   cd /explorer/
   docker compose up
   ```

### Agora Language Compiler

1. Build the compiler:
   ```bash
   cd /agora_lang_compiler/
   make
   ```

2. Compile an Agora smart contract:
   ```bash
   ./main examples/your_contract.agora
   ```

### VM Debugger

1. Build the debugger:
   ```bash
   cd /core_protocol/vm_debugger
   cargo build --release
   ```

2. Run the debugger:
   ```bash
   ./target/release/vm_debugger
   ```

## Architecture

### Core Protocol

- **Node/Peer**: Each node maintains its own copy of the blockchain state and interacts with other nodes to reach consensus.

- **Consensus Mechanism (Proof-of-Work)**: Nodes mine new blocks by solving computational puzzles, ensuring consistency across the network.

- **Transactions**: Every action in the protocol is triggered by a transaction:
  - **Normal Currency Transaction**: Transfers funds from one address to another.
  - **Contract Creation**: Deploys a new smart contract.
  - **Contract Execution**: Executes functions of a deployed smart contract.

### State Management

The protocol maintains several states (global_state, receipts, storage/contract) as a pair of in-memory and on-disk data structures:
  - **In-memory Structure**: A custom Merkle-Patricia-Trie for efficient operations
  - **On-disk Structure**: A key-value database implemented at `/core_protocol/database`

This dual structure allows for:
  - Fast write/lookup operations on the trie (in-memory)
  - State reversion capabilities using the database
  - Efficient proof generation for verification

> Note: Both the database and virtual machine are implemented in Rust and exposed as libraries via FFI.

### Virtual Machine

- **Server Module**: Responsible for spawning VM instances and handling data requests from the chain (such as address state or contract storage).

- **State Machine Design**: The VM core is implemented as a state machine, allowing it to request data and be polled by the server synchronously and in a single-threaded manner. This design is crucial for reliable FFI interaction.

### Agora Language Compiler

The compiler transforms Agora smart contract language into bytecode for the virtual machine:

- **Lexer**: Performs lexical analysis to convert source code into tokens
- **Parser**: Builds abstract syntax tree (AST) from tokens
- **AST**: Defines the hierarchical structure of the program
- **Code Generator**: Produces executable bytecode for the virtual machine

### VM Debugger

The debugger provides an interactive environment for inspecting and debugging virtual machine bytecode execution:

- **Interactive Commands**:
  - Step-by-step execution
  - Continuous execution until breakpoint
  - State rollback
  - Program inspection with `stack`, `memory`, `storage` commands

- **Debugging Features**:
  - Breakpoint management at specific program counter positions
  - Real-time stack and memory inspection
  - Current instruction display with program counter tracking
  - Full program instruction listing with execution pointer

- **State Visualization**:
  - Stack values displayed with indices
  - Memory shown in hexadecimal format with addresses
  - Storage entries as key-value pairs
  - Clear status indicators for VM state and operations