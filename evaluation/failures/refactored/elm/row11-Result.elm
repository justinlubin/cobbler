extractNewNumberFact : Result String NumberFact -> Msg
extractNewNumberFact result =
    case result of
        Err error ->
            ErrorGettingNumberFact error

        Ok numberFact ->
            NewNumberFact numberFact
