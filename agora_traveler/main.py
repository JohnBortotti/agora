from flask import Flask, request, render_template
import requests

app = Flask(__name__)

@app.route('/')
def home():
  node = request.args.get('node', default="http://node1:8080")
  start_query = request.args.get('start', default="0")
  end_query = request.args.get('end', default="20")
  blocks = []
  try:
    r = requests.get(f'{node}/blocks?start={start_query}&end={end_query}')
    blocks = r.json()
  except:
    blocks = []

  return render_template(
    'index.html',
    title='Chain explorer',
    blocks=blocks
  )

@app.route('/consensus')
def consensus():
    nodes = ["http://node1:8080", "http://node2:8080", "http://node3:8080"]

    start_query = request.args.get('start', default="0")
    end_query = request.args.get('end', default="20")

    chains = []
    for node in nodes:
        blocks = []
        try:
          r = requests.get(f'{node}/blocks?start={start_query}&end={end_query}')
          blocks = r.json()
        except:
          blocks = []

        chains.append({"node": node, "blocks": blocks})

    reference_chain = chains[0]['blocks']
    validation_results = []

    for ref_block in reference_chain:
        block_consensus = {"index": ref_block['index'], "block": ref_block, "is_valid": True, "diverging_peers": []}

        for peer in chains[1:]:
            peer_block = next((b for b in peer['blocks'] if b['index'] == ref_block['index']), None)

            if not peer_block or peer_block != ref_block:
                block_consensus["is_valid"] = False
                block_consensus["diverging_peers"].append({
                    "peer": peer['node'],
                    "peer_block": peer_block if peer_block else {"index": "N/A", "hash": "N/A"}
                })

        validation_results.append(block_consensus)

    return render_template(
        'consensus.html',
        title='Consensus',
        reference_chain=reference_chain,
        validation_results=validation_results
    )

@app.route('/account')
def account():
  node = request.args.get('node', default="http://node1:8080")
  account_query = request.args.get('account', default="")
  account = None
  try:
    r = requests.get(f'{node}/account?account={account_query}')
    if r.status_code == 200:
      account = r.json()
    else:
      account = {"error": f"Error fetching from {node}"}
  except:
      account = {"error": f"Error fetching from {node}"}

  return render_template(
    'account.html',
    title='Account explorer',
    account=account
  )

@app.route('/account-consensus')
def account_consensus():
    nodes = ["http://node1:8080", "http://node2:8080", "http://node3:8080"]
    account_query = request.args.get('account', default="")
    accounts = []

    for node in nodes:
        account = {}
        try:
            r = requests.get(f'{node}/account?account={account_query}')
            if r.status_code == 404:
                account = {"error": f"Account not found on {node} (404)"}
            else:
                account = r.json()
        except requests.exceptions.RequestException as e:
            account = {"error": f"Node {node} unreachable: {str(e)}"}

        accounts.append(account)

    reference_account = accounts[0]
    validation_results = []

    for idx, account in enumerate(accounts):
        if "error" in account:
            validation_results.append({
                "node": nodes[idx],
                "status": "error",
                "details": account["error"]
            })
        elif account == reference_account:
            validation_results.append({
                "node": nodes[idx],
                "status": "valid",
                "account": account,
                "details": "Account state matches"
            })
        else:
            validation_results.append({
                "node": nodes[idx],
                "status": "mismatch",
                "account": account,
                "details": "Account state differs"
            })

    return render_template(
        'account-consensus.html',
        title='Account consensus',
        reference_account=reference_account,
        validation_results=validation_results
    )

if __name__ == '__main__':
  app.run(host='0.0.0.0', port=5000)
