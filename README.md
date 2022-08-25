# Component-based refactoring

## Installation instructions

1. [Install OCaml and opam](https://ocaml.org/docs/up-and-running)
2. Run `make switch`
3. Restart your shell
4. Run `make dev-deps deps`

To execute `bin/main.ml`, run `make exec`.

To run the tests in the `tests` directory, run `make test`.

## Project structure

- `bin` implements the "main" executable function.
- `lib` implements the synthesis procedure; two key things to look out for are
  that 1) the main entry point is `lib/synthesize.ml`, and 2) many important
  types are defined in `lang.ml`.
- `test` defines tests.
