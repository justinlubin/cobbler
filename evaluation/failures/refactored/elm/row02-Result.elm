tryToEdit : Int -> Result Error TUnit -> Msg
tryToEdit index result =
    case result of
        Ok _ ->
            GotSelectedMsg index StartEditingText

        Err err ->
            LogError (domErrToString err)

-- *** Result.catamorphism (auto)

tryToEdit : Int -> Result Error TUnit -> Msg
tryToEdit index result =
    result
        |> Result.catamorphism (\y -> LogError (domErrToString y)) (\_ -> GotSelectedMsg index StartEditingText)