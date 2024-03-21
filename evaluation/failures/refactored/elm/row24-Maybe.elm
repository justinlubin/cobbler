andMaybeEventWithArg : Maybe (a -> msg) -> a -> (msg -> Attribute msg) -> List (Attribute msg) -> List (Attribute msg)
andMaybeEventWithArg maybeMsg arg toEvent attrs =
    case maybeMsg of
        Nothing ->
            attrs

        Just msg ->
            toEvent (msg arg) :: attrs

-- *** Maybe of function type

andMaybeEventWithArg : Maybe (a -> msg) -> a -> (msg -> Attribute msg) -> List (Attribute msg) -> List (Attribute msg)
andMaybeEventWithArg maybeMsg arg toEvent attrs =
    maybeMsg
        |> Maybe.map (\f -> toEvent (f arg) :: attrs)
        |> Maybe.withDefault attrs