FROM python:3.11
USER root
ENV DEBIAN_FRONTEND = noninteractive
RUN apt-get update && \
  apt-get install -y software-properties-common && \
  apt-get install -y opam
RUN opam init --disable-sandboxing && \
  opam switch create 4.14.0 && \
  opam install dune && \
  eval $(opam env)
RUN apt-get install -y nodejs npm && \
  npm install -g elm-format
COPY . /cbr/
WORKDIR /cbr/backend
RUN apt-get install -y autoconf && \
  opam install -y --deps-only --with-test --with-doc . && \
  opam install -y utop ocamlformat.0.22.4 ocaml-lsp-server sexp && \
  opam pin ocamlformat 0.22.4
WORKDIR /cbr/frontend
RUN pip install -r requirements.txt
WORKDIR /cbr
CMD ["/bin/bash", "-c", "eval $(opam env);python3 frontend/login.py;frontend/main.py benchmark --language python --sample-limit 10000 --unsafe-eval data/py.tsv"]