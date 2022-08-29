.PHONY: build
build:
	dune build

.PHONY: test
test:
	dune runtest

.PHONY: exec
exec:
	dune exec bin/main.exe

.PHONY: repl
repl:
	dune utop lib

.PHONY: switch
switch:
	opam update && opam switch create 4.14.0

.PHONY: deps
deps:
	opam install --deps-only --with-test --with-doc .

.PHONY: dev-deps
dev-deps:
	opam install utop ocamlformat.0.22.4 ocaml-lsp-server sexp

.PHONY: fmt
fmt:
	dune build @fmt --auto-promote

.PHONY: watch
watch:
	dune build --watch --terminal-persistence=clear-on-rebuild

.PHONY: watch-test
watch-test:
	dune runtest --watch --terminal-persistence=clear-on-rebuild
