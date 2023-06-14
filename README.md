# Component-based refactoring

[![Tests](https://github.com/justinlubin/component-based-refactoring/actions/workflows/workflow.yml/badge.svg)](https://github.com/justinlubin/component-based-refactoring/actions/workflows/workflow.yml)

This project contains the source code for the Garnet program synthesizer, which implements the component-based refactoring algorithm.

This repository is split into two main sections: the backend (implemented in OCaml and located in the `backend` directory) and the frontend (implemented in Python and located in the `frontend` directory).

The `garnet` script at the project's root directory glues everything together and provides the user-facing CLI for the synthesizer.

To use the `garnet` script, you first need to install its dependencies (see below).

Once that is done, see `garnet --help`.

## Installing dependencies

### Installing OCaml dependencies

1. If you haven't already, [install OCaml and opam](https://ocaml.org/docs/up-and-running)
2. From the project's root directory, run `cd backend`
3. Run `make switch`
4. Restart your shell
5. Run `make dev-deps deps`

### Installing Python dependencies

1. If you haven't already, [install Python](https://www.python.org/)
2. From the project's root directory, run `cd frontend`
3. *Optional: If you use virtual environments, create a new one for this project now. The next step will use `pip` to install the necessary Python packages.*
4. Run `make deps`

## Running tests

To run the backend tests, run `make test` from the `backend` subdirectory.

To run the frontend tests, run `make test` from the `frontend` subdirectory.
