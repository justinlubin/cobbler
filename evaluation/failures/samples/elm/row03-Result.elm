getOrCrash : Result String r -> r
getOrCrash res =
    case res of
        Err e ->
            Debug.crash e

        Ok r ->
            r
