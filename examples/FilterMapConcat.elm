main : (String -> Bool) -> (String -> List Int) -> List String -> List Int
main p f list =
  case list of
    [] -> []
    head :: tail -> if p head then f head ++ main p f tail else main p f tail
