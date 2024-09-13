from flask import Flask, render_template

app = Flask(__name__)

@app.route('/')
def home():

  blocks = [
    {
        "id": 123456,
        "hash": "0xabcdef1234567890",
        "timestamp": "2024-09-12 10:22:33",
        "miner": "0x1234abcd...",
        "transactions": [
            {"id": 1, "hash": "0xabcdef01", "from": "0xaaa...", "to": "0xbbb..."},
            {"id": 2, "hash": "0xabcdef02", "from": "0xccc...", "to": "0xddd..."},
        ]
    },
    {
        "id": 123457,
        "hash": "0x9876543210abcdef",
        "timestamp": "2024-09-12 10:24:12",
        "miner": "0x4321dcba...",
        "transactions": [
            {"id": 3, "hash": "0x9876abcd", "from": "0xaaa...", "to": "0xbbb..."},
            {"id": 4, "hash": "0x9876efgh", "from": "0xccc...", "to": "0xddd..."},
        ]
    }
  ]

  return render_template(
    'index.html',
    title='Agora Traveler',
    blocks=blocks
  )

if __name__ == '__main__':
  app.run(host='0.0.0.0', port=5000)
