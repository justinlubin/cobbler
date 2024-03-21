merge : Result a a -> a
merge r =
    case r of
        Ok rr ->
            rr

        Err rr ->
            rr

-- *** Result.catamorphism (auto)

merge : Result a a -> a
merge r =
    r
        |> Result.catamorphism (\y -> y) (\x -> x)