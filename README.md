# Component-based refactoring

[![Tests](https://github.com/justinlubin/component-based-refactoring/actions/workflows/workflow.yml/badge.svg)](https://github.com/justinlubin/component-based-refactoring/actions/workflows/workflow.yml)

## Installation instructions

1. [Install OCaml and opam](https://ocaml.org/docs/up-and-running)
2. Run `make switch`
3. Restart your shell
4. Run `make dev-deps deps`

To execute `bin/main.ml`, run `make exec`.

To run the tests in the `tests` directory, run `make test`.

## Project structure

- `bin` implements the "main" executable function.
- `lib/cbr_framework` implements the component-based refactoring framework
- `lib/cbr_fp` instantiates the component-based refactoring framework in the
  domain of functional programming combinators
- `lib/cbr_numpy` instantiates the component-based refactoring framework in the
  domain of NumPy combinators
- `lib/util` provides general utilities
- `test` defines tests
