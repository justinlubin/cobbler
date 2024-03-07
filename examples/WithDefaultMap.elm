main : (Int -> Int) -> Maybe Int -> Int
main f mx =
 case mx of
   Nothing -> 0
   Just x -> f (f x)
