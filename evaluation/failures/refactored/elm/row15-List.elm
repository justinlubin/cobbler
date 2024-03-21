extendLeft : List a -> List1 a -> List1 a
extendLeft list list1 =
    case list of
        [] ->
            list1

        first :: rest ->
            List1 first (rest ++ toList list1)

-- *** List.uncons (auto)

extendLeft : List a -> List1 a -> List1 a
extendLeft list list1 =
    list
        |> List.uncons list1 (\x -> \y -> List1 x (y ++ toList list1))