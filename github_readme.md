# Cobbler

[![Tests](https://github.com/justinlubin/component-based-refactoring/actions/workflows/workflow.yml/badge.svg)](https://github.com/justinlubin/component-based-refactoring/actions/workflows/workflow.yml)

This project contains the source code for the Cobbler program synthesizer.
Cobbler refactors programs to use library functions. It currently supports:

- Refactoring [Elm](https://elm-lang.org/) programs to use functions like those
  found in the Elm Standard Library (e.g. functional combinators like `map` and
  `filter`), and
- Refactoring Python programs to use the [NumPy](https://numpy.org/)
  high-performance computing library.

For information about how this project is structured, please see
`ARCHITECTURE.md`.

For information about how to install the dependencies for this project, please
see `DEPENDENCIES.md`.

Once depencies are installed, you can run Cobbler via the `cobbler` script in
the root of this repository.

For help, run `./cobbler --help`.

## Resource acknowledgments

- Thanks to
  [Patrick Gillespie](http://patorjk.com/)
  for the
  [Text to ASCII Art Generator](http://patorjk.com/software/taag)
  and `myflix` for the "Sweet" ASCII art font.
