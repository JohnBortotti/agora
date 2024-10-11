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

    let get_account state address =
      match State.get state address with
      | None -> None
      | Some acc_data -> Some (decode (RLP.decode acc_data))

    (* TODO: fix the case we sender or receiver is the same address as miner_addr *)
    (* TODO: instead of result, return: transaction receipt *)
    (* TODO: pass the entire State.t, not just the trie, so the function can apply 
       changes direct on serialized state
      *)
    let apply_transaction miner_addr global_state contract_state tx vm_fun =
      let open Digestif.SHA256 in
      let open Result in

      let (let*) = Result.bind in

      let total_cost = tx.amount + (tx.gas_limit * tx.gas_price) in
      
      let* sender_account = (match get_account global_state tx.sender with
        | None -> Error "Sender account not found"
        | Some acc ->
            if acc.balance < total_cost then
              Error "Insufficient balance to cover amount and fees"
            else if acc.nonce <> tx.nonce then
              Error "Invalid nonce"
            else
              Ok acc
      ) in

      let* miner_account = (match get_account global_state miner_addr with
        | None -> Ok {
            address = miner_addr;
            balance = 0;
            nonce = 0;
            storage_root = "";
            code_hash = "";
          }
        | Some acc -> Ok acc
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

      let state_after_fees = 
        State.set global_state sender_account.address (encode updated_sender_account)
        |> fun f -> State.set f miner_account.address (encode updated_miner_account)
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

          let* contract_account = (match get_account global_state contract_address with
            | Some _ -> Error "Contract account already exists"
            | None -> Ok {
              address = contract_address;
              balance = tx.amount;
              nonce = 0;
              storage_root = "";
              (* TODO: get the correct code, we also have the deploy code *)
              code_hash = code_hash
          }) in

          Printf.printf "Contract account created: %s\n" contract_account.address;

          (* storing the contract account on global_state: <contract_address, ...>*)
          let state_with_contract = 
            State.set state_after_fees contract_address (encode contract_account)
          in

          (* storing the actual contract code on contract_state: <code_hash, full_code>*)
          let new_contract_state =
            State.set contract_state code_hash (`String tx.payload)
          in

          Ok(state_with_contract, new_contract_state)
      end
      else 
        begin
          let* receiver_account = (match get_account global_state tx.receiver with
            | None -> Ok {
                address = tx.receiver;
                balance = tx.amount;
                nonce = 0;
                storage_root = "";
                code_hash = "";
              }
            | Some acc -> Ok acc
          ) in

          let new_state = 
            State.set state_after_fees receiver_account.address (encode receiver_account)
          in

          (* contract execution *)
          if receiver_account.code_hash <> "" then begin
            let vm_res = vm_fun tx in
            Printf.printf "[Ocaml tx_apply] VM %s finished execution\n" vm_res;
            (* TODO: return unused gas to sender, and remove from miner *)
            Ok(new_state, contract_state)
          end
          (* normal transaction *)
          else 
            Ok(new_state, contract_state)
      end

    (* TODO: each transaction should generate a log with topics,
     this is where we use the Error of apply_transaction *)
    let rec apply_transactions miner_addr global_state contract_state transactions vm_fun = 
      match transactions with
      | tx :: rest ->
          (match apply_transaction miner_addr global_state contract_state tx vm_fun with
          | Ok (new_global_state, new_contract_state) -> 
              Printf.printf "Transaction executed ok!\n\n"; 
              apply_transactions miner_addr new_global_state new_contract_state rest vm_fun
          | Error err -> 
              Printf.printf "Transaction execution error: %s\n" err;
              apply_transactions miner_addr global_state contract_state rest vm_fun)
      | _ -> (global_state, contract_state)

    let apply_transaction_coinbase global_state tx =
      match get_account global_state tx.receiver with
      | None ->
        let new_receiver_account = {
          address = tx.receiver;
          balance = tx.amount;
          nonce = 0;
          storage_root = "";
          code_hash = "";
        } in
        let new_state = 
          State.set global_state new_receiver_account.address (encode new_receiver_account) 
        in
        Ok(new_state)
      | Some receiver_account ->
        let updated_receiver_account = {
          receiver_account with balance = receiver_account.balance + tx.amount
        } in
        let new_state =
          State.set global_state receiver_account.address (encode updated_receiver_account) in
        Ok(new_state)

  let apply_block_transactions 
    miner_addr global_state contract_state transactions vm_fun =
    match transactions with
    | coinbase :: [] -> 
      (match apply_transaction_coinbase global_state coinbase with
        | Ok (new_state) ->
            (new_state, contract_state)
        | _ -> 
            apply_transactions miner_addr global_state contract_state transactions vm_fun)
    | coinbase :: transactions ->
      (match apply_transaction_coinbase global_state coinbase with
        | Ok (new_state) ->
            apply_transactions miner_addr new_state contract_state transactions vm_fun
        | _ -> 
            apply_transactions miner_addr global_state contract_state transactions vm_fun)
    | _ -> (global_state, contract_state)
end
