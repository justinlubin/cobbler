resultToJson : Result String a -> Decoder a
resultToJson r =
    case r of
        Ok o ->
            Json.Decode.succeed o

        Err e ->
            Json.Decode.fail e
