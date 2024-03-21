okOrCrash : Result String a -> a
okOrCrash result =
    case result of
        Ok a ->
            a

        Err str ->
            Debug.crash str

-- *** Result.catamorphism (auto)

okOrCrash : Result String a -> a
okOrCrash result =
    result
        |> Result.catamorphism Debug.crash (\x -> x)