# Component-based refactoring

[![Tests](https://github.com/justinlubin/component-based-refactoring/actions/workflows/workflow.yml/badge.svg)](https://github.com/justinlubin/component-based-refactoring/actions/workflows/workflow.yml)

## Installation instructions

**Important note on Python:**
For the benchmarking script, it is recommended to create a new Python
environment to avoid polluting the global Python environment (e.g. via `conda`).
The `make deps` command will use `pip` to install dependencies, so if you use
`conda`, make sure to run `conda install pip` in your environment first. After
running `make deps`,
**you may also need to run `pip install -U pip setuptools`, as per
[this GitHub issue](https://github.com/conda/conda/issues/11931)!**

1. [Install OCaml and opam](https://ocaml.org/docs/up-and-running)
2. Run `make switch`
3. Restart your shell
4. Run `make dev-deps deps`

To execute `bin/main.ml`, run `make exec`.

To run the tests in the `tests` directory, run `make test`.

## Benchmarking

To build the benchmarking script, run `make build`. To run benchmarking, run `make benchmark`.

Test synthesizer inputs should be placed in `lib/cbr_numpy/data/benchmarking/targets.ipynb`. Inputs should be of the form

```
#[test name]
[environment]
#synth
[body]
```

where the body ends in an expression. Benchmarking data is generated in `lib/cbr_numpy/data/benchmarking/benchmarks.csv`.

## Project structure

- `bin` implements the "main" executable function.
- `lib/cbr_framework` implements the component-based refactoring framework
- `lib/cbr_fp` instantiates the component-based refactoring framework in the
  domain of functional programming combinators
- `lib/cbr_numpy` instantiates the component-based refactoring framework in the
  domain of NumPy combinators
- `lib/util` provides general utilities
- `test` defines tests
