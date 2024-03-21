resultToJson : Result String a -> Decoder a
resultToJson r =
    case r of
        Ok o ->
            Json.Decode.succeed o

        Err e ->
            Json.Decode.fail e

-- *** Result.catamorphism (auto)

resultToJson : Result String a -> Decoder a
resultToJson r =
    r
        |> Result.catamorphism Json.Decode.fail Json.Decode.succeed