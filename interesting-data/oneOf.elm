oneOf : List (Maybe.Maybe a) -> Maybe.Maybe a
oneOf maybes =
    case maybes of
        [] ->
            Maybe.Nothing

        maybe :: rest ->
            case maybe of
                Maybe.Nothing ->
                    oneOf rest

                Maybe.Just _ ->
                    maybe