findPreviousHelp : a -> Int -> (a -> String) -> String -> List a -> Maybe (Previous a)
findPreviousHelp previous index entryId currentId entries =
    case entries of
        [] ->
            Nothing

        next :: rest ->
            case entryId next == currentId of
                True ->
                    Just (Previous index previous)

                False ->
                    findPreviousHelp next (index + 1) entryId currentId rest
