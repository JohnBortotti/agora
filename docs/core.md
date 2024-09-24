# core protocol

This documentation provides an overview of the core protocol, 
the core protocol of Agora blockchain network includes:

- node/peer: each node acts as a peer, maintaining its own copy of the blockchain and interacting with others.
- consensus mechanism (proof-of-work): nodes mine new blocks by solving computational puzzles, ensuring consensus across the network.
- transactions:
  - normal currency transaction: transfers funds from one user to another. (DONE)
  - contract creation: deploys a new smart contract. (TODO)
  - contract execution: executes functions of a deployed smart contract. (TODO)
- state management:
  - account state: tracks funds, nonce, storage, and code hash for each account. (DONE)
  - contract storage: maintains key-value storage specific to each contract (TODO)
- virtual machine and Agora lang (TODO)
