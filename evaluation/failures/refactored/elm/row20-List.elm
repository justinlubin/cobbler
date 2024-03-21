cartesianProduct : List (List a) -> List (List a)
cartesianProduct ll =
    case ll of
        [] ->
            [ [] ]

        xs :: xss ->
            lift2 (::) xs (cartesianProduct xss)

-- *** List.catamorphism (auto)

cartesianProduct : List (List a) -> List (List a)
cartesianProduct ll =
    ll
        |> List.catamorphism [ [] ] (lift2 (::))