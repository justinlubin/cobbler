showResult : (t -> List String) -> Result String t -> List String
showResult show res =
    case res of
        Ok x ->
            show x

        Err err ->
            [ err ]
