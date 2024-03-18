processDeathResponse : Result Error String -> Msg
processDeathResponse result =
    case result of
        Ok sid ->
            StatsDeath sid

        Err err ->
            Error (toString err)
