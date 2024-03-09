# Architecture

This repository is split into two main sections: the backend (implemented in
OCaml and located in the `backend` directory) and the frontend (implemented in
Python and located in the `frontend` directory).

The `cobbler` script at the project's root directory glues everything together
and provides the user-facing CLI for the synthesizer.

To use the `cobbler` script, you first need to install its dependencies. If you
are using the provided PLDI 2024 virtual machine, this has already been done for
you. Otherwise, please see `DEPENDENCIES.md`.

## Running tests

To run the backend tests, run `make test` from the `backend` subdirectory.

To run the frontend tests, run `make test` from the `frontend` subdirectory.
