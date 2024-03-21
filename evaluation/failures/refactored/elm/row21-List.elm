join : List (Generator a) -> Generator (List a)
join generators =
    case generators of
        [] ->
            Random.constant []

        first :: rest ->
            Random.map2 (::) first (join rest)
