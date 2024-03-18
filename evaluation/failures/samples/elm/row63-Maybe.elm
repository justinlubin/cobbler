fetchOnInput : Maybe (String -> msg) -> Attribute msg
fetchOnInput input =
    case input of
        Nothing ->
            Html.Styled.Attributes.class ""

        Just msg ->
            Html.Styled.Events.onInput msg
