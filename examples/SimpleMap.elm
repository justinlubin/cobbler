main : (Int -> Int) -> Maybe Int -> Maybe Int
main f mx =
 case mx of
   Nothing -> Nothing
   Just x -> Just (f x)
