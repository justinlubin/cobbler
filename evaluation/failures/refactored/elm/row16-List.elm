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

-- *** List.foldl (early-cutoff)

elementAt : Int -> List a -> Maybe a
elementAt index list =
    list
        |> List.foldl
            ( \x acc ->
                case acc of
                    (1, Nothing) ->
                        (0, Just x)

                    (i, m) ->
                        (i - 1, m)
            )
            (index, Nothing)
        |> Tuple.second