# Component-based refactoring

TODO: Must remove HuggingFace key!

[![Tests](https://github.com/justinlubin/component-based-refactoring/actions/workflows/workflow.yml/badge.svg)](https://github.com/justinlubin/component-based-refactoring/actions/workflows/workflow.yml)

This project contains the source code for the `cobbler` program synthesizer, which implements the component-based refactoring algorithm.

This repository is split into two main sections: the backend (implemented in OCaml and located in the `backend` directory) and the frontend (implemented in Python and located in the `frontend` directory).

The `cobbler` script at the project's root directory glues everything together and provides the user-facing CLI for the synthesizer.

To use the `cobbler` script, you first need to install its dependencies (see below).

Once that is done, see `./cobbler --help`.

## Installing dependencies

### Installing OCaml dependencies

1. If you haven't already, [install OCaml and opam](https://ocaml.org/docs/up-and-running)
2. From the project's root directory, run `cd backend`
3. Run `make switch`
4. Restart your shell
5. Run `make dev-deps deps`

### Installing Python dependencies

1. If you haven't already, [install Python](https://www.python.org/) version 3.11 or greater
2. From the project's root directory, run `cd frontend`
3. *Optional: If you use virtual environments, create a new one for this project now. The next step will use `pip` to install the necessary Python packages.*
4. Run `make deps`

### Other dependencies

- This project also depends on [elm-format](https://github.com/avh4/elm-format) (tested with version 0.8.7), which is installable via `npm`

## Running the evaluation

- To run the evaluation described in the paper, run the `run_evaluation.sh`
  script in the project's root repository

## Running the analysis scripts

The scripts in the `analysis/` directory analyze the benchmarking results. They require the following Python dependencies:

- `numpy`
- `matplotlib`
- `pandas`

## Running tests

To run the backend tests, run `make test` from the `backend` subdirectory.

To run the frontend tests, run `make test` from the `frontend` subdirectory.

## Acknowledgments

- Thanks to
  [Patrick Gillespie](http://patorjk.com/)
  for the
  [Text to ASCII Art Generator](http://patorjk.com/software/taag)
  and `myflix` for the "Sweet" ASCII art font.
