from flask import Flask, request, render_template
import requests

app = Flask(__name__)

@app.route('/')
def home():

  node = request.args.get('node', default="http://node1:8080")
  start_query = request.args.get('start', default="0")
  end_query = request.args.get('end', default="20")

  r = requests.get(f'{node}/blocks?start={start_query}&end={end_query}')
  blocks = r.json()

  return render_template(
    'index.html',
    title='Chain explorer',
    blocks=blocks
  )

@app.route('/consensus')
def compare():
    nodes = ["http://node1:8080", "http://node2:8080", "http://node3:8080"]

    start_query = request.args.get('start', default="0")
    end_query = request.args.get('end', default="20")

    chains = []
    for node in nodes:
        r = requests.get(f'{node}/blocks?start={start_query}&end={end_query}')
        blocks = r.json()
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

if __name__ == '__main__':
  app.run(host='0.0.0.0', port=5000)
