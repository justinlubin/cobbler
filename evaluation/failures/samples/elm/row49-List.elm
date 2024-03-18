appendLineRef : Int -> List String -> List String
appendLineRef lineRef lines =
    case lines of
        [] ->
            []

        latestLine :: olderLines ->
            (latestLine ++ (" (" ++ (String.fromInt lineRef ++ ")"))) :: olderLines
