result : Result err a -> a
result resultA =
    case resultA of
        Ok a ->
            a

        Err err ->
            Debug.crash ("Could not unwrap result. Error: " ++ toString err)

-- *** Result.catamorphism (auto)

result : Result err a -> a
result resultA =
    resultA
        |> Result.catamorphism (\y -> Debug.crash ("Could not unwrap result. Error: " ++ toString y)) (\x -> x)