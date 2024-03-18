unwrap : Result a a -> a
unwrap result =
    case result of
        Ok value ->
            value

        Err value ->
            value
