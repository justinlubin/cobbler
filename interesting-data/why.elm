getAuthHeaders : Maybe.Maybe String -> List Http.Header
getAuthHeaders maybeKey =
    case maybeKey of
        Maybe.Just apiKey ->
            [ Http.header "Authorization" ((++) "token " apiKey)
            ]

        Maybe.Nothing ->
            []
