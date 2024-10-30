# setup

## node/peer

1. go to `/core_protocol/database` and run `cargo build --release`
2. go to `/core_protocol/virtual_machine` and run `cargo build --release`
3. go to `/core_protocol/` and run containers with `docker compose up`
 
## node explorer

1. go to `/explorer/` and run container with `docker compose up`

# core protocol

- **node/peer**: each node maintais its own copy of the blockchain state and interact with others nodes to reach consensus.

- **consensus mechanism (proof-of-work)**: nodes mine new blocks by solving computational puzzles, ensuring consensus across the network.

- every action in the protocol is triggered by a **transaction**:
  - **normal currency transaction**: transfers funds from one address to another.
  - **contract creation**: deploys a new smart contract.
  - **contract execution**: executes functions of a deployed smart contract.

## state

each state (global_state, receipts, storage/contract) is mantained as a pair of in-memory and on-disk data structure:
  - the in-memory structure is a custom merkle-patricia-trie
  - the on-disk structure is a key-value database implemented at `/core_protocol/database`

this structure allows for easy and fast write/lookup on the trie (in-memory) and enables state reversion using the database.

* both the database and virtual_machine are implemented in Rust, and exposed as library via FFI

## virtual_machine

- virtual machine executions are handled by a **server** module, wich is responsible to spawn VMs and handle data requests from chain (like getting and address state, or fetching contract's storage)

- the virtual machine core is implemented as a **state machine**, this design allows it to request data and be polled by the server in a synchronous and single-threaded manner, which is required to avoid issues with FFI.
