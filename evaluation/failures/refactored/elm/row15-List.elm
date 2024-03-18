extendLeft : List a -> List1 a -> List1 a
extendLeft list list1 =
    case list of
        [] ->
            list1

        first :: rest ->
            List1 first (rest ++ toList list1)
