result : Result err a -> a
result resultA =
    case resultA of
        Ok a ->
            a

        Err err ->
            Debug.crash ("Could not unwrap result. Error: " ++ toString err)
