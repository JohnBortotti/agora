# Agora

Agora is a distributed computing protocol powered by blockchain technology.

## repository structure

- **core_protocol**:  core protocol implementation.
    - **core**: node/peer implementation handling network communication and consensus mechanisms.
    - **database**: key-value database used to manage the blockchain state.
    - **virtual_machine**: virtual machine for executing smart contracts.
    
- **explorer**: a simple node explorer webpage, written using the Flask framework, for visualizing network activity.

- **agora_lang_compiler**: compiler implementation for the Agora smart contract language.
    - **lexer**: performs lexical analysis to convert source code into tokens.
    - **parser**: builds abstract syntax tree from tokens.
    - **ast**: defines the abstract syntax tree structure.
    - **code_gen**: generates executable code for the virtual machine.

- **docs**: documentation and notes about the project.

check out [docs](./docs/README.md)

## TODO

### Lang Compiler
- [ ] Function dispatcher (ABI)
  - [ ] Read payload data
  - [ ] Mappings (storage read/write)

### Smart Contracts
- [ ] Persistent storage
- [ ] Add bloom filter to track events

### Node
- [ ] Optimizations and fixes
  - [ ] Optimize block broadcasting (remove cyclical requests)
  - [ ] Implement singleton secp256k1 context
