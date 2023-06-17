nextElem : a -> List a -> Maybe.Maybe a
nextElem el list =
    case list of
        curr :: rest ->
            case (==) curr el of
                Basics.True ->
                    List.head rest

                Basics.False ->
                    nextElem el rest

        [] ->
            Maybe.Nothing

