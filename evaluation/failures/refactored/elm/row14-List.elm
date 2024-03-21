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

-- *** List.catamorphism (auto)

truncateList : (a -> Bool) -> List a -> List a
truncateList filter list =
    list
        |> List.catamorphism []
            (\x ->
                \y ->
                    case filter x of
                        True ->
                            [ x ]

                        False ->
                            x :: y
            )