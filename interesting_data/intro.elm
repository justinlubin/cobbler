m : (String -> Bool) -> (String -> List Int) -> List String -> List Int
m predicate fn xs =
  case xs of
    [] -> []
    hd :: tl ->
      if predicate hd then
        fn hd ++ m predicate fn tl
      else
        m predicate fn tl
