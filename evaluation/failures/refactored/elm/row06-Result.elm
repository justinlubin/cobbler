resultToParser : Result String a -> Parser a
resultToParser result =
    case result of
        Err e ->
            Parser.Future.problem e

        Ok v ->
            Parser.Future.succeed v
