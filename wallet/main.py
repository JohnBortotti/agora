from flask import Flask, request, render_template
import requests

from flask import Flask, request, jsonify
import hashlib
import json
from coincurve import PrivateKey, PublicKey

app = Flask(__name__)

@app.route('/generate-transaction', methods=['POST'])
def generate_transaction():
  receiver = request.form['receiver']
  amount = int(request.form['amount'])
  gas_limit = int(request.form['gas_limit'])
  gas_price = int(request.form['gas_price'])
  nonce = int(request.form['nonce'])
  payload = request.form['payload']
  private_key_hex = request.form['private_key']

  private_key = PrivateKey.from_hex(private_key_hex)
  public_key = private_key.public_key.format(compressed=False).hex()

  transaction_data = {
    "sender": public_key,
    "receiver": receiver,
    "amount": amount,
    "gas_limit": gas_limit,
    "gas_price": gas_price,
    "nonce": nonce,
    "payload": payload
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
    "signature": signature_hex
  }

  return jsonify(transaction_payload)

@app.route('/send-transaction', methods=['POST'])
def send_transaction():
  data = request.json
  node_address = data.get('node_address')
  transaction = data.get('transaction')

  print("\n\n")
  print(node_address)
  print("\n\n")
  print(transaction)

  if not node_address or not transaction:
    return jsonify({"status": "error", "message": "Missing node address or transaction data."}), 400

  try:
    print(f"Sending transaction to {node_address} with payload:")
    print(json.dumps(transaction, indent=4))

    response = requests.post(f'{node_address}/transaction', json=transaction)
    if response.status_code == 200:
      return jsonify({"status": "success", "message": "Transaction sent successfully."})
    else:
      return jsonify({"status": "error", "message": "Failed to send transaction to node."}), 500
  except requests.exceptions.RequestException as e:
    return jsonify({"status": "error", "message": f"Error sending transaction: {str(e)}"}), 500


@app.route('/')
def home():
  return render_template(
    'index.html',
    title="Agora wallet"
  )

if __name__ == '__main__':
  app.run(host='0.0.0.0', port=5000)
