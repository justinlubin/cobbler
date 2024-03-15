fromResult : Result String a -> Decoder a
fromResult result =
    case result of
        Ok a ->
            Json.Decode.succeed a

        Err errorMessage ->
            Json.Decode.fail errorMessage
