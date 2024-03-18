titleUpdate : Result String PlainText -> String
titleUpdate result =
    case result of
        Ok _ ->
            "Flashback ⚡"

        Err e ->
            "! " ++ (e ++ " | Flashback ⚡")
