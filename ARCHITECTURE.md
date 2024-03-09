# Architecture

This file documents the high-level architecture of this repository. Further
documentation can be found in the code modules themselves as interface comments.

This repository is split into two main sections: the backend (implemented in
OCaml and located in the `backend` directory) and the frontend (implemented in
Python and located in the `frontend` directory). The `evaluation` directory
contains evaluation materials for the paper associated with this tool; further
information about the evaluation can be found in that directory and in the
artifact evaluation README.

The `cobbler` script at the project's root directory glues everything together
and provides the user-facing CLI for the synthesizer.

To use the `cobbler` script, you first need to install its dependencies. If you
are using the provided PLDI 2024 virtual machine, this has already been done for
you. Otherwise, please see `DEPENDENCIES.md`.

## The backend

The backend is split into three subcomponents:

- `bin` (the "main" executable, i.e., the backend entry points),
- `lib` (the actual code that implements the program synthesis), and
- `test` (the backend tests).

The file `bin/main.ml` is a good place to start to see how all the library
components fit together.

Within `lib`, there are four further subcomponents:

- `cbr_fp`: component-based refactoring for Elm ("fp" = functional programming)
- `cbr_numpy`: component-based refactoring for Python / NumPy
- `cbr_framework`: shared code for the above two instantiations relating to
  the component-based refactoring framework
- `util`: utility functions

A good starting point to explore `cbr_fp` is the file `synthesis.mli`, which
lays out the core types for the Elm synthesis problem. The corresponding
implementation file, `synthesis.ml` is the central part of the implementation,
implementing both canonicalization and enumerative synthesis. Additionally,
`recursion_scheme.ml(i)` implements our recursion scheme extractor and
`fusion.ml(i)` implements our catamorphism fusion (which is standard).

Similarly, a good starting point to explore `cbr_numpy` are the files
`np_synthesis.mli` and `np_synthesis.ml`, which are analagously central to the
synthesis algorithm for NumPy. Additionally of note, the file
`partial_eval.ml(i)` implements our partial evaluator and serves a similar role
to catamorphism fusion for Elm.

Most types relating to the languages that these two instantiations work with are
in their corresponding `lang.ml` files.

## The frontend

The frontend is relatively simple, with a main entry point of `main.py`. The
`cobbler` script in the root of the repository is actually a symlink to this
file.

The `main.py` is a bit long, but its functionality is relatively
compartmentalized according to the subcommand that is passed to the script. A
good starting point for understanding the frontend is to look at the end of
the `main.py` script in the `if __name__ == "__main__"` conditional.

Other files of note include:

- `db_iter.py` (downloads input programs from The Stack),
- `extract.py` (extracts synthesis problems from input programs), and
- `run_backend.py` (actually calls the backend to perform synthesis).

Lastly, tests are included in the `test` subdirectory.

## Running tests

To run the backend tests, run `make test` from the `backend` subdirectory.

To run the frontend tests, run `make test` from the `frontend` subdirectory.
