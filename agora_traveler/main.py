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
    title='Agora Traveler',
    blocks=blocks
  )

if __name__ == '__main__':
  app.run(host='0.0.0.0', port=5000)
