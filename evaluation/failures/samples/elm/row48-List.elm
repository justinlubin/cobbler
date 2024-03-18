indexOfhelper : List a -> a -> Int -> Maybe Int
indexOfhelper lst elem offset =
    case lst of
        [] ->
            Nothing

        x :: xs ->
            case x == elem of
                True ->
                    Just offset

                False ->
                    indexOfhelper xs elem (offset + 1)
