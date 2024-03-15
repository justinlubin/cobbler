truncateList : (a -> Bool) -> List a -> List a
truncateList filter list =
    case list of
        [] ->
            []

        h :: li ->
            case filter h of
                True ->
                    [ h ]

                False ->
                    h :: truncateList filter li
