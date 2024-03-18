merge : Result a a -> a
merge r =
    case r of
        Ok rr ->
            rr

        Err rr ->
            rr
