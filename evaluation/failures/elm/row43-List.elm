updateIndex : Int -> (a -> a) -> List a -> List a
updateIndex n func list =
    case list of
        [] ->
            []

        x :: xs ->
            case n <= 0 of
                True ->
                    func x :: xs

                False ->
                    x :: updateIndex (n - 1) func xs
