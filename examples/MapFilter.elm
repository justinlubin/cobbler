main : (Int -> Bool) -> (Int -> Int) -> List Int -> List Int
main p f xs =
  case xs of
    [] -> []
    hd :: tl -> if p hd then f (f hd) :: main p f tl else main p f tl
