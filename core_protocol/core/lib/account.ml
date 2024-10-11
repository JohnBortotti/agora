open State
open Transaction
 
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

  let get_account state address: t option =
    match State.get state address with
    | None -> None
    | Some acc_data -> Some (decode (RLP.decode acc_data))

  (* TODO: fix the case we sender or receiver is the same address as miner_addr *)
  (* TODO: instead of result, return: transaction receipt *)
  (* TODO: pass the entire State.t, not just the trie, so the function can apply 
      changes direct on serialized state
    *)
  let apply_transaction miner_addr global_state contract_state tx vm_fun: receipt =
    let open Digestif.SHA256 in

    let total_cost = tx.amount + (tx.gas_limit * tx.gas_price) in

    let mocked_receipt_ok = {
      result = Success;
      gas_used = 0;
      logs = [];
      bloom_filter = "";
      contract_address = None;
    } in

    let mocked_receipt_err = {
      result = Failure;
      gas_used = 0;
      logs = [];
      bloom_filter = "";
      contract_address = None;
    } in

    (* let* sender_account = (match get_account global_state tx.sender with
      | None -> Error "Sender account not found"
      | Some acc ->
        if acc.balance < total_cost then
          Error "Insufficient balance to cover amount and fees"
        else if acc.nonce <> tx.nonce then
          Error "Invalid nonce"
        else
          Ok acc
    ) in *)

    match get_account global_state tx.sender with
    | None -> mocked_receipt_err
    | Some sender_account ->
      let miner_account = (match get_account global_state miner_addr with
        | None -> {
          address = miner_addr;
          balance = 0;
          nonce = 0;
          storage_root = "";
          code_hash = "";
        }
        | Some acc -> acc
      ) in

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
      State.set global_state sender_account.address (encode updated_sender_account);
      State.set global_state miner_account.address (encode updated_miner_account);

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

          (match get_account global_state contract_address with 
            | Some _ -> mocked_receipt_err
            | None ->
              let contract_account = {
                address = contract_address;
                balance = tx.amount;
                nonce = 0;
                storage_root = "";
                code_hash = code_hash;
              } in

              Printf.printf "Contract account created: %s\n" contract_account.address;

              (* storing the contract account on global_state: <contract_address, ...>*)
              State.set global_state contract_address (encode contract_account);

              (* storing the actual contract code on contract_state: <code_hash, full_code>*)
              State.set contract_state code_hash (`String tx.payload);

              mocked_receipt_ok)
      end
      else 
        begin
          let receiver_account = 
            match get_account global_state tx.receiver with
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
          State.set global_state receiver_account.address (encode receiver_account);

          (* contract execution *)
          if receiver_account.code_hash <> "" then begin
            let vm_res = vm_fun tx in
            Printf.printf "[Ocaml tx_apply] VM %s finished execution\n" vm_res;
            (* TODO: return unused gas to sender, and remove from miner *)
            mocked_receipt_ok
          end
          (* normal transaction *)
          else 
            mocked_receipt_ok
      end

  (* TODO: each transaction should generate a log with topics,
    this is where we use the Error of apply_transaction *)
  let rec apply_transactions miner_addr global_state contract_state transactions vm_fun: receipt list = 
    match transactions with
    | tx :: rest ->
        let receipt = apply_transaction miner_addr global_state contract_state tx vm_fun in
        receipt :: apply_transactions miner_addr global_state contract_state rest vm_fun
    | [] -> []

  let apply_transaction_coinbase global_state tx =
    let mocked_receipt = {
      result = Success;
      gas_used = 0;
      logs = [];
      bloom_filter = "";
      contract_address = None;
    } in
    match get_account global_state tx.receiver with
    | None ->
      let new_receiver_account = {
        address = tx.receiver;
        balance = tx.amount;
        nonce = 0;
        storage_root = "";
        code_hash = "";
      } in
      State.set global_state new_receiver_account.address (encode new_receiver_account);
      mocked_receipt
    | Some receiver_account ->
      let updated_receiver_account = {
        receiver_account with balance = receiver_account.balance + tx.amount
      } in
      State.set global_state receiver_account.address (encode updated_receiver_account);
      mocked_receipt

  let apply_block_transactions 
    miner_addr global_state contract_state transactions vm_fun =
    match transactions with
    | coinbase :: [] -> 
        let coinbase_receipt = apply_transaction_coinbase global_state coinbase in
        coinbase_receipt :: []
    | coinbase :: transactions ->
        let coinbase_receipt = apply_transaction_coinbase global_state coinbase in
        let tx_receipts = apply_transactions miner_addr global_state contract_state transactions vm_fun in
        coinbase_receipt :: tx_receipts
    | _ -> []
end