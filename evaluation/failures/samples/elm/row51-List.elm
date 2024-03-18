valInList : List a -> a -> Int -> Bool
valInList l elem pos =
    case l of
        [] ->
            False

        x :: xs ->
            (x == elem) || valInList xs elem (pos + 1)
