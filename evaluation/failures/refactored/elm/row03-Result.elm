getOrCrash : Result String r -> r
getOrCrash res =
    case res of
        Err e ->
            Debug.crash e

        Ok r ->
            r

-- *** Result.catamorphism (auto)

getOrCrash : Result String r -> r
getOrCrash res =
    res
        |> Result.catamorphism Debug.crash (\x -> x)