fetchOnClick : Int -> Maybe (Int -> msg) -> Attribute msg
fetchOnClick index message =
    case message of
        Nothing ->
            Html.Styled.Attributes.class ""

        Just msg ->
            Html.Styled.Events.onClick (msg index)
