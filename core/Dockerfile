FROM ocaml/opam:latest

RUN sudo apt-get update && sudo apt-get install -y \
    build-essential \
    m4 \
    zlib1g-dev \
    libgmp-dev \
    pkg-config \
		libsecp256k1-dev \
    && opam install -y dune cryptokit \
    && opam install -y dune digestif \
    && opam install -y dune lwt \
		&& opam install cohttp-lwt-unix cohttp-async \
		&& opam install yojson \
		&& opam install alcotest \
		&& opam install secp256k1 --unlock-base \
    && sudo apt-get clean

WORKDIR /home/opam/app

COPY . .

CMD ["dune", "exec", "bin/main.exe"]
