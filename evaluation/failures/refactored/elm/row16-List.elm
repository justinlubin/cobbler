elementAt : Int -> List a -> Maybe a
elementAt index list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            case index == 1 of
                True ->
                    Just x

                False ->
                    elementAt (index - 1) xs
