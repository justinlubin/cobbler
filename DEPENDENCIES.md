# Installing dependencies

## Installing OCaml dependencies

1. If you haven't already, [install OCaml and opam](https://ocaml.org/docs/up-and-running)
2. From the project's root directory, run `cd backend`
3. Run `make switch`
4. Restart your shell
5. Run `make dev-deps deps`

## Installing Python dependencies

1. If you haven't already, [install Python](https://www.python.org/) version
   3.11 or greater
2. From the project's root directory, run `cd frontend`
3. *Optional: If you use virtual environments, create a new one for this project
   now. The next step will use `pip` to install the necessary Python packages.*
4. Run `make deps` (for convenience, this will also install the dependencies
   necessary to analyze the evaluation data)

## Additional dependencies

- This project also depends on [elm-format](https://github.com/avh4/elm-format)
  (tested with version 0.8.7), which is installable via `npm`
