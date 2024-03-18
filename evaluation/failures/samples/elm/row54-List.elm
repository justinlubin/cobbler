all : List (Result x a) -> Result x (List a)
all list =
    case list of
        [] ->
            Ok []

        x :: xs ->
            Result.map2 (::) x (all xs)
