listFirstIndex : (a -> Bool) -> Int -> List a -> Maybe Int
listFirstIndex test currentIndex list =
    case list of
        [] ->
            Nothing

        headElement :: remainingElements ->
            case test headElement of
                True ->
                    Just currentIndex

                False ->
                    listFirstIndex test (currentIndex + 1) remainingElements
