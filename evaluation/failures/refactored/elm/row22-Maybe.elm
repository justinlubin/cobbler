fetchOnClick : Int -> Maybe (Int -> msg) -> Attribute msg
fetchOnClick index message =
    case message of
        Nothing ->
            Html.Styled.Attributes.class ""

        Just msg ->
            Html.Styled.Events.onClick (msg index)

-- *** Maybe of function type

fetchOnClick : Int -> Maybe (Int -> msg) -> Attribute msg
fetchOnClick index message =
    message
        |> Maybe.map (\f -> Html.Styled.Events.onClick (f index))
        |> Maybe.withDefault (Html.Styled.Attributes.class "")