fromErr : Result e a -> Maybe e
fromErr x =
    case x of
        Err x ->
            Just x

        Ok _ ->
            Nothing
