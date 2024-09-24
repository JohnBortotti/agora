from flask import Flask, render_template, request, jsonify
import requests
import json
import hashlib
from coincurve import PrivateKey

app = Flask(__name__)

def send_jsonrpc_request(node_url, method, params, request_id=None):
    headers = {'Content-Type': 'application/json'}
    if request_id is None:
        request_id = 1

    payload = {
        "jsonrpc": "2.0",
        "method": method,
        "params": params,
        "id": request_id
    }

    try:
        response = requests.post(node_url, headers=headers, json=payload)
        response_json = response.json()
        if 'error' in response_json:
            return None, response_json['error']
        else:
            return response_json.get('result'), None
    except requests.exceptions.RequestException as e:
        return None, {"code": -1, "message": str(e)}

@app.route("/")
def home():
    node = request.args.get("node", default="http://node1:8080")
    start_query = int(request.args.get("start", default="0"))
    end_query = int(request.args.get("end", default="20"))
    blocks = []

    result, error = send_jsonrpc_request(
        node,
        "get_blocks",
        [start_query, end_query],
        request_id=1
    )

    if error:
        blocks = []
    else:
        blocks = result

    return render_template("index.html", title="Chain Explorer", blocks=blocks)

@app.route("/consensus")
def consensus():
    nodes = ["http://node1:8080", "http://node2:8080", "http://node3:8080"]

    start_query = int(request.args.get("start", default="0"))
    end_query = int(request.args.get("end", default="20"))

    chains = []
    for idx, node in enumerate(nodes):
        result, error = send_jsonrpc_request(
            node,
            "get_blocks",
            [start_query, end_query],
            request_id=idx + 1
        )

        if error:
            blocks = []
        else:
            blocks = result

        chains.append({"node": node, "blocks": blocks})

    reference_chain = chains[0]["blocks"] if chains else []
    validation_results = []

    for ref_block in reference_chain:
        block_consensus = {
            "index": ref_block["index"],
            "block": ref_block,
            "is_valid": True,
            "diverging_peers": [],
        }

        for peer in chains[1:]:
            peer_block = next(
                (b for b in peer["blocks"] if b["index"] == ref_block["index"]), None
            )

            if not peer_block or peer_block != ref_block:
                block_consensus["is_valid"] = False
                block_consensus["diverging_peers"].append(
                    {
                        "peer": peer["node"],
                        "peer_block": peer_block
                        if peer_block
                        else {"index": "N/A", "hash": "N/A"},
                    }
                )

        validation_results.append(block_consensus)

    return render_template(
        "consensus.html",
        title="Chain Consensus",
        reference_chain=reference_chain,
        validation_results=validation_results,
    )

@app.route("/account")
def account():
    node = request.args.get("node", default="http://node1:8080")
    account_query = request.args.get("account", default="")
    account = None

    if not account_query:
        account = {"error": "No account address provided."}
        return render_template("account.html", title="Account Explorer", account=account)

    result, error = send_jsonrpc_request(
        node,
        "get_account",
        [account_query],
        request_id=1
    )

    if error:
        account = {"error": f"Error fetching from {node}: {error.get('message', '')}"}
    else:
        account = result

    return render_template("account.html", title="Account Explorer", account=account)

@app.route("/account-consensus")
def account_consensus():
    nodes = ["http://node1:8080", "http://node2:8080", "http://node3:8080"]
    account_query = request.args.get("account", default="")
    accounts = []

    if not account_query:
        return render_template(
            "account-consensus.html",
            title="Account Consensus",
            reference_account=None,
            validation_results=[],
        )

    for idx, node in enumerate(nodes):
        result, error = send_jsonrpc_request(
            node,
            "get_account",
            [account_query],
            request_id=idx + 1
        )

        if error:
            account = {"error": f"Error fetching from {node}: {error.get('message', '')}"}
        else:
            account = result

        accounts.append({"node": node, "account": account})

    reference_account = accounts[0]["account"] if accounts else None
    validation_results = []

    for idx, entry in enumerate(accounts):
        account = entry["account"]
        node = entry["node"]
        if "error" in account:
            validation_results.append(
                {"node": node, "status": "error", "details": account["error"]}
            )
        elif account == reference_account:
            validation_results.append(
                {
                    "node": node,
                    "status": "valid",
                    "account": account,
                    "details": "Account state matches",
                }
            )
        else:
            validation_results.append(
                {
                    "node": node,
                    "status": "mismatch",
                    "account": account,
                    "details": "Account state differs",
                }
            )

    return render_template(
        "account-consensus.html",
        title="Account Consensus",
        reference_account=reference_account,
        validation_results=validation_results,
    )

@app.route("/generate-transaction", methods=["POST"])
def generate_transaction():
    receiver = request.form["receiver"]
    amount = int(request.form["amount"])
    gas_limit = int(request.form["gas_limit"])
    gas_price = int(request.form["gas_price"])
    nonce = int(request.form["nonce"])
    payload = request.form["payload"]
    private_key_hex = request.form["private_key"]

    private_key = PrivateKey.from_hex(private_key_hex)
    public_key = private_key.public_key.format(compressed=False).hex()

    transaction_data = {
        "sender": public_key,
        "receiver": receiver,
        "amount": amount,
        "gas_limit": gas_limit,
        "gas_price": gas_price,
        "nonce": nonce,
        "payload": payload,
    }

    transaction_str = (
        f"{transaction_data['sender']}"
        f"{transaction_data['receiver']}"
        f"{transaction_data['amount']}"
        f"{transaction_data['gas_limit']}"
        f"{transaction_data['gas_price']}"
        f"{transaction_data['nonce']}"
        f"{transaction_data['payload']}"
    )

    transaction_hash = hashlib.sha256(transaction_str.encode()).digest()
    signature = private_key.sign_recoverable(transaction_hash, hasher=None)
    signature_hex = signature.hex()

    transaction_payload = {
        "hash": transaction_hash.hex(),
        "sender": public_key,
        "receiver": receiver,
        "amount": amount,
        "gas_limit": gas_limit,
        "gas_price": gas_price,
        "nonce": nonce,
        "payload": payload,
        "signature": signature_hex,
    }

    return jsonify(transaction_payload)

@app.route("/send-transaction", methods=["POST"])
def send_transaction():
    data = request.json
    node_address = data.get("node_address")
    transaction = data.get("transaction")

    if not node_address or not transaction:
        return (
            jsonify(
                {
                    "status": "error",
                    "message": "Missing node address or transaction data.",
                }
            ),
            400,
        )

    result, error = send_jsonrpc_request(
        node_address,
        "submit_transaction",
        [transaction],
        request_id=1
    )

    if error:
        return (
            jsonify(
                {
                    "status": "error",
                    "message": f"Error sending transaction: {error.get('message', '')}",
                }
            ),
            500,
        )
    else:
        return jsonify(
            {"status": "success", "message": "Transaction sent successfully."}
        )

@app.route("/transaction")
def transaction():
    return render_template("transaction.html", title="New Transaction")

@app.route("/keys")
def keys():
    private_key = PrivateKey()
    public_key = private_key.public_key

    private_key_hex = private_key.to_hex()
    public_key_hex = public_key.format(compressed=False).hex()

    return render_template(
        "keys.html",
        title="New Key Pair",
        private_key=private_key_hex,
        public_key=public_key_hex,
    )

if __name__ == "__main__":
    app.run(host="0.0.0.0", port=5000)

