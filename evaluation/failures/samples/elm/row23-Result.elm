processLifts : Result Error (List Lift) -> Msg
processLifts result =
    case result of
        Ok a ->
            UpdateHandle <| HandleLifts a

        Err err ->
            UpdateHandle <| HandleError err
