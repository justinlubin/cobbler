field : (a -> String) -> (a -> b) -> String -> List a -> Maybe.Maybe b
field getKey getValue name record =
    case record of
        [] ->
            Maybe.Nothing

        x :: xs ->
            case (==) (getKey x) name of
                Basics.True ->
                    Maybe.Just (getValue x)

                Basics.False ->
                    field getKey getValue name xs
