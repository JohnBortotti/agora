FROM ocaml/opam:debian-ocaml-5.2

ENV OPAMJOBS=4

# install system dependencies in a single step
RUN sudo apt-get update && sudo apt-get install -y \
    build-essential \
    m4 \
    zlib1g-dev \
    libgmp-dev \
    pkg-config \
    libsecp256k1-dev \
    libffi-dev \
    && sudo apt-get clean \
    && sudo rm -rf /var/lib/apt/lists/*

# install all OCaml dependencies in a single step for better caching
RUN opam update && opam install -y \
    dune \
    cryptokit \
    digestif \
    lwt \
    cohttp-lwt-unix \
    cohttp-async \
    yojson \
    alcotest \
    ctypes \
    ctypes-foreign \
    && opam install -y secp256k1 --unlock-base

WORKDIR /home/opam/app

# copy database and VM files
COPY ./database/target/release/ /home/opam/database/
COPY ./virtual_machine/target/release/ /home/opam/virtual_machine/

# copy application code
COPY ./core /home/opam/app

# create required directories
RUN mkdir -p /home/opam/db-data/global-state/ \
    /home/opam/db-data/contract-state/ \
    /home/opam/db-data/receipt-state \
    /home/opam/debug

CMD ["dune", "exec", "bin/main.exe"]
