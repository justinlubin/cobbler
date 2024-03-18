andMaybeEventWithArg : Maybe (a -> msg) -> a -> (msg -> Attribute msg) -> List (Attribute msg) -> List (Attribute msg)
andMaybeEventWithArg maybeMsg arg toEvent attrs =
    case maybeMsg of
        Nothing ->
            attrs

        Just msg ->
            toEvent (msg arg) :: attrs
