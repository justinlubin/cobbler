fromList : List a -> Maybe (NonEmptyList a)
fromList list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            Just (wrap x xs)
