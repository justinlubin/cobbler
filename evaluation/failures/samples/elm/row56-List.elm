things : List (Maybe a) -> List a
things xs =
    case xs of
        [] ->
            []

        x :: xs ->
            case x of
                Just x ->
                    x :: things xs

                Nothing ->
                    things xs
