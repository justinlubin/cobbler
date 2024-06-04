<div align="center">
  <img src="https://jlubin.net/assets/cobbler-banner.png" alt="cobbler">
</div>

# `cobbler`

[![Tests](https://github.com/justinlubin/component-based-refactoring/actions/workflows/workflow.yml/badge.svg)](https://github.com/justinlubin/component-based-refactoring/actions/workflows/workflow.yml)

`cobbler` refactors programs to use library functions.

It currently supports refactoring:

- **[Elm](https://elm-lang.org/) programs** to use functions like those found in
  the Elm Standard Library (e.g. `map` and `filter`); specifically, the ones
  defined in [this file](backend/bin/Stdlib.elm)
- **Python programs** to use the [NumPy](https://numpy.org/) high-performance 
  computing library

**How to run:** First, install `cobbler`'s dependencies by following the
instructions in [`DEPENDENCIES.md`](DEPENDENCIES.md); then, you can run
`cobbler` via the `cobbler` script in the root of this repository.

**For help:** Run `./cobbler --help`.

**For information about how `cobbler` works:** Please see our
[PLDI 2024 paper preprint](https://jlubin.net/assets/pldi24-preprint.pdf)!

## Examples

### Elm

#### withDefault/map

Input:

```elm
main : (Int -> Int) -> Maybe Int -> Int
main f mx =
 case mx of
   Nothing -> 0
   Just x -> f (f x)
```

Output:

```elm
main : (Int -> Int) -> Maybe Int -> Int
main f mx =
    mx
        |> Maybe.map (\x -> f (f x))
        |> Maybe.withDefault 0
```

#### concat/map/filter

Input:

```elm
main : (String -> Bool) -> (String -> List Int) -> List String -> List Int
main p f list =
  case list of
    [] -> []
    head :: tail -> if p head then f head ++ main p f tail else main p f tail
```

Output:

```elm
main : (String -> Bool) -> (String -> List Int) -> List String -> List Int
main p f list =
    list
        |> List.filter p
        |> List.map f
        |> List.concat
```

### Python

#### Dot product

Input:

```python
s = 0
for i in range(len(x)):
    s += x[i] * y[i]
s
```

Output:

```python
s = np.sum(np.multiply(x, y[:len(x)]))
s
```

#### Rolling sum

Input:

```python
y = np.zeros(len(x) - WINDOW_SIZE + 1)
for i in range(len(y)):
    s = 0
    for j in range(WINDOW_SIZE):
        s += x[i + j]
    y[i] = s
y
```

Output:

```python
y = np.convolve(x, np.full(WINDOW_SIZE, 1), mode="valid")
y
```

## Additional information

- For information about how this project is structured, please see
[`ARCHITECTURE.md`](ARCHITECTURE.md).
- For information about the artifact evaluation process for our PLDI 2024 paper,
please see [`ARTIFACT_EVALUATION.md`](ARTIFACT_EVALUATION.md).

## Art acknowledgments

- Thanks to [Anna
    Christenson](https://www.linkedin.com/in/anna-christenson-a51a08213/) for the logo!
- Thanks to
  [Patrick Gillespie](http://patorjk.com/)
  for the
  [Text to ASCII Art Generator](http://patorjk.com/software/taag)
  and `myflix` for the "Sweet" ASCII art font.
