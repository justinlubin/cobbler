fetchOnInput : Maybe (String -> msg) -> Attribute msg
fetchOnInput input =
    case input of
        Nothing ->
            Html.Styled.Attributes.class ""

        Just msg ->
            Html.Styled.Events.onInput msg

-- *** Maybe of function type

fetchOnInput : Maybe (String -> msg) -> Attribute msg
fetchOnInput input =
    input
        |> Maybe.map (\f -> Html.Styled.Events.onInput f)
        |> Maybe.withDefault (Html.Styled.Attributes.class "")