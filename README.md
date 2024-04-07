# Cobbler

[![Tests](https://github.com/justinlubin/component-based-refactoring/actions/workflows/workflow.yml/badge.svg)](https://github.com/justinlubin/component-based-refactoring/actions/workflows/workflow.yml)

This project contains the source code for the Cobbler program synthesizer.
Cobbler refactors programs to use library functions. It currently supports:

- Refactoring [Elm](https://elm-lang.org/) programs to use functions like those
  found in the Elm Standard Library (e.g. functional combinators like `map` and
  `filter`), and
- Refactoring Python programs to use the [NumPy](https://numpy.org/)
  high-performance computing library.

For information about how to install the dependencies for this project, please
see `DEPENDENCIES.md`.

Once depencies are installed, you can run Cobbler via the `cobbler` script in
the root of this repository.

For help, run `./cobbler --help`.

## Examples

### Elm

#### withDefault/map

**Input:**

```
main : (Int -> Int) -> Maybe Int -> Int
main f mx =
 case mx of
   Nothing -> 0
   Just x -> f (f x)
```

**Output:**

```
main : (Int -> Int) -> Maybe Int -> Int
main f mx =
    mx
        |> Maybe.map (\x -> f (f x))
        |> Maybe.withDefault 0
```

#### concat/map/filter

**Input:**

```
main : (String -> Bool) -> (String -> List Int) -> List String -> List Int
main p f list =
  case list of
    [] -> []
    head :: tail -> if p head then f head ++ main p f tail else main p f tail
```

**Output:**

```
main : (String -> Bool) -> (String -> List Int) -> List String -> List Int
main p f list =
    list
        |> List.filter p
        |> List.map f
        |> List.concat
```

### Python

#### Dot product

**Input:**

```
s = 0
for i in range(len(x)):
    s += x[i] * y[i]
s
```

**Output:**

```
s = np.sum(np.multiply(x, y[:len(x)]))
s
```

#### Rolling sum

**Input:**

```
y = np.zeros(len(x) - WINDOW_SIZE + 1)
for i in range(len(y)):
    s = 0
    for j in range(WINDOW_SIZE):
        s += x[i + j]
    y[i] = s
y
```

**Output:**

```
y = np.convolve(x, np.full(WINDOW_SIZE, 1), mode="valid")
y
```

## Additional information

For information about how this project is structured, please see
`ARCHITECTURE.md`.

For information about the artifact evaluation process for our PLDI 2024 paper,
please see `ARTIFACT_EVALUATION.md`.

## Resource acknowledgments

- Thanks to
  [Patrick Gillespie](http://patorjk.com/)
  for the
  [Text to ASCII Art Generator](http://patorjk.com/software/taag)
  and `myflix` for the "Sweet" ASCII art font.
