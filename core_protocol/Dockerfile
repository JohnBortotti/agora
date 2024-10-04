FROM ocaml/opam:latest

# RUN sudo apt-get update && sudo apt-get install -y \
RUN sudo apt-get install -y \
    build-essential \
    m4 \
    zlib1g-dev \
    libgmp-dev \
    pkg-config \
    libsecp256k1-dev \
    libffi-dev \
    && opam install -y dune cryptokit \
    && opam install -y dune digestif \
    && opam install -y dune lwt \
    && opam install cohttp-lwt-unix cohttp-async \
    && opam install yojson \
    && opam install alcotest \ 
    && opam install secp256k1 --unlock-base \
    && opam install ctypes ctypes-foreign \
    && sudo apt-get clean

WORKDIR /home/opam/app

COPY ./database/target/release/ /home/opam/database/

COPY ./virtual_machine/target/release/ /home/opam/virtual_machine/

COPY ./core /home/opam/app

RUN mkdir /home/opam/db-data/
RUN mkdir /home/opam/db-data/global-state/

CMD ["dune", "exec", "bin/main.exe"]
