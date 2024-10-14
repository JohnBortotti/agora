open State
open Transaction
open Lwt.Syntax
 
module Account = struct
  type t = {
    address: string;
    balance: int;
    nonce: int;
    storage_root: string;
    code_hash: string;
  }

  let string_of_account account =
    Printf.sprintf "Address: %s\nBalance: %d\nNonce: %d\nStorage Root: %s\nCode Hash: %s\n"
      account.address
      account.balance
      account.nonce
      account.storage_root
      account.code_hash

  let account_to_json account =
    `Assoc [
      ("address", `String account.address);
      ("balance", `Int account.balance);
      ("nonce", `Int account.nonce);
      ("storage_root", `String account.storage_root);
      ("code_hash", `String account.code_hash);
    ]

  let encode account =
   `List [
      `String account.address;
      `String (string_of_int account.balance);
      `String (string_of_int account.nonce);
      `String account.storage_root;
      `String account.code_hash;
    ]

  let decode = function
    | `List [`String address; `String balance; `String nonce; `String storage_root; `String code_hash] ->
        { address; balance = int_of_string balance; nonce = int_of_string nonce; storage_root; code_hash }
    | _ -> failwith "Invalid RLP encoding for account"

  let get_account trie address: t option =
    match MKPTrie.lookup trie address with
    | None -> None
    | Some (Leaf (_, acc_data))
    | Some (Branch (_, Some acc_data)) -> Some ((decode (RLP.decode acc_data)))
    | _ -> None

  (* TODO: fix the case we sender or receiver is the same address as miner_addr *)
  let apply_transaction miner_addr global_state_trie storage_trie receipt_trie tx vm_fun: 
    (MKPTrie.t * MKPTrie.t * MKPTrie.t) =
    let open Digestif.SHA256 in

    let total_cost = tx.amount + (tx.gas_limit * tx.gas_price) in

    match get_account global_state_trie tx.sender with
    | None -> 
      let receipt = {
        transaction_hash = tx.hash;
        result = Failure;
        message = "Sender account not found";
        gas_used = 0;
        logs = [];
        bloom_filter = "";
        contract_address = None;
      } in
      let updated_receipt_trie = MKPTrie.insert receipt_trie tx.hash (encode_receipt receipt) in
      (global_state_trie, storage_trie, updated_receipt_trie)
    | Some sender_account ->
      let miner_account = match get_account global_state_trie miner_addr with
      | None -> {
        address = miner_addr;
        balance = 0;
        nonce = 0;
        storage_root = "";
        code_hash = "";
      }
      | Some acc -> acc
      in

      let updated_sender_account = { 
        sender_account with
        balance = sender_account.balance - total_cost;
        nonce = sender_account.nonce + 1;
      } in

      let updated_miner_account = {
        miner_account with
        balance = miner_account.balance + (tx.gas_limit * tx.gas_price);
      } in
      
      (* state after fees *)
      let state_trie_after_fees = 
        MKPTrie.insert global_state_trie sender_account.address (encode updated_sender_account)
        |> fun f -> MKPTrie.insert f miner_account.address (encode updated_miner_account)
      in

      (* contract creation *)
      if tx.receiver = "0" then 
        begin
          (* TODO: fix contract_address generation *)
          (* TODO: 
            - run vm and execute the contract_init()
            - the resulting opcode is stored at contract_state db (code_hash, program)
            - update the account with code_hash (hashed program)
            - store the storage_root on contract_state db (hash the trie root)
          *)
          let contract_address = to_hex (digest_string
            (RLP.encode(`List[`String tx.sender; `String (string_of_int tx.nonce)]))
          ) in

          let code_hash = (to_hex (digest_string tx.payload)) in

          (match get_account global_state_trie contract_address with
            | Some _ -> 
              let receipt = {
                transaction_hash = tx.hash;
                result = Failure;
                message = "Contract address already in use";
                gas_used = 0;
                logs = [];
                bloom_filter = "";
                contract_address = None;
              } in
              let updated_receipt_trie = MKPTrie.insert receipt_trie tx.hash (encode_receipt receipt) in
              (state_trie_after_fees, storage_trie, updated_receipt_trie)
            | None -> 
              let contract_account = {
                address = contract_address;
                balance = tx.amount;
                nonce = 0;
                storage_root = "";
                code_hash = code_hash;
              } in

              (* storing the contract account on global_state: <contract_address, ...>*)
              let state_trie_with_contract = 
                MKPTrie.insert state_trie_after_fees contract_address (encode contract_account)
              in

              (* storing the actual contract code on contract_state: <code_hash, full_code>*)
              let contract_code_state =
                MKPTrie.insert storage_trie code_hash (`String tx.payload)
              in

              Printf.printf "contract created: %s\n" contract_address;

              (* storing the receipt on receipt_state*)
              let receipt = {
                transaction_hash = tx.hash;
                result = Success;
                message = "Contract created";
                gas_used = 0;
                logs = [];
                bloom_filter = "";
                contract_address = Some contract_address;
              } in
              let updated_receipt_trie = MKPTrie.insert receipt_trie tx.hash (encode_receipt receipt) in
              (state_trie_with_contract, contract_code_state, updated_receipt_trie))
      end
      else 
        begin
          let receiver_account =
            match get_account global_state_trie tx.receiver with
            | None -> {
                address = tx.receiver;
                balance = tx.amount;
                nonce = 0;
                storage_root = "";
                code_hash = "";
              }
            | Some acc -> acc
          in

          (* send amount to receiver *)
          let state_trie_after_pay_receiver = 
            MKPTrie.insert state_trie_after_fees receiver_account.address (encode receiver_account)
          in

          (* contract execution *)
          if receiver_account.code_hash <> "" then begin
            let vm_res = vm_fun tx in
            Printf.printf "[Ocaml tx_apply] VM %s finished execution\n" vm_res;
            (* TODO: add VM logs on receipt *)

            let mocked_receipt_ok = {
              transaction_hash = tx.hash;
              result = Success;
              message = "Contract executed";
              gas_used = 0;
              logs = [];
              bloom_filter = "";
              contract_address = Some receiver_account.address;
            } in
            let updated_receipt_trie = MKPTrie.insert receipt_trie tx.hash 
              (encode_receipt mocked_receipt_ok) 
            in
            (state_trie_after_pay_receiver, storage_trie, updated_receipt_trie)
          end
          (* normal transaction *)
          else 
            let receipt = {
              transaction_hash = tx.hash;
              result = Success;
              message = "Normal transaction";
              gas_used = 0;
              logs = [];
              bloom_filter = "";
              contract_address = None;
            } in
            let updated_receipt_trie = MKPTrie.insert receipt_trie tx.hash (encode_receipt receipt) in
            (state_trie_after_pay_receiver, storage_trie, updated_receipt_trie)
      end

  let rec apply_transactions miner_addr global_state contract_state receipt_trie transactions vm_fun: (MKPTrie.t * MKPTrie.t * MKPTrie.t) = 
    match transactions with
    | tx :: rest ->
        let (new_global_state, new_contract_state, new_receipt_trie) = 
          apply_transaction miner_addr global_state contract_state receipt_trie tx vm_fun in 
        let (final_global_state, final_contract_state, final_receipt_trie) = 
          apply_transactions miner_addr new_global_state new_contract_state new_receipt_trie rest vm_fun in
        (final_global_state, final_contract_state, final_receipt_trie)
    | [] -> (global_state, contract_state, receipt_trie)

  let apply_transaction_coinbase global_state_trie receipt_trie tx: (MKPTrie.t * MKPTrie.t) =
    let receipt = {
      transaction_hash = tx.hash;
      result = Success;
      message = "Coinbase transaction";
      gas_used = 0;
      logs = [];
      bloom_filter = "";
      contract_address = None;
    } in
    let updated_receipt_trie = MKPTrie.insert receipt_trie tx.hash (encode_receipt receipt) in  

    match get_account global_state_trie tx.receiver with
    | None ->
      let new_receiver_account = {
        address = tx.receiver;
        balance = tx.amount;
        nonce = 0;
        storage_root = "";
        code_hash = "";
      } in
      
      let state_trie_with_receiver = 
        MKPTrie.insert global_state_trie new_receiver_account.address (encode new_receiver_account) in
          
      (state_trie_with_receiver, updated_receipt_trie)
    | Some receiver_account ->
      let updated_receiver_account = {
        receiver_account with balance = receiver_account.balance + tx.amount
      } in
      let state_trie_with_receiver = 
        MKPTrie.insert global_state_trie receiver_account.address (encode updated_receiver_account) in
      (state_trie_with_receiver, updated_receipt_trie)
       
  let apply_block_transactions 
    miner_addr global_state_trie contract_trie receipt_trie transactions vm_fun =
    match transactions with
    | coinbase :: [] -> 
        let (new_global_state, coinbase_receipt_trie) = apply_transaction_coinbase global_state_trie receipt_trie coinbase in
        (new_global_state, contract_trie, coinbase_receipt_trie)
    | coinbase :: transactions ->
        let (new_global_state, coinbase_receipt_trie) = apply_transaction_coinbase global_state_trie receipt_trie coinbase in
        let (final_global_state, final_contract_trie, final_receipt_trie) = 
          apply_transactions miner_addr new_global_state contract_trie coinbase_receipt_trie transactions vm_fun in
        (final_global_state, final_contract_trie, final_receipt_trie)
    | _ -> (global_state_trie, contract_trie, receipt_trie)
end
