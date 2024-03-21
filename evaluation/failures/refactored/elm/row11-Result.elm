extractNewNumberFact : Result String NumberFact -> Msg
extractNewNumberFact result =
    case result of
        Err error ->
            ErrorGettingNumberFact error

        Ok numberFact ->
            NewNumberFact numberFact

-- *** Result.catamorphism (auto)

extractNewNumberFact : Result String NumberFact -> Msg
extractNewNumberFact result =
    result
        |> Result.catamorphism (\y -> ErrorGettingNumberFact y) (\x -> NewNumberFact x)