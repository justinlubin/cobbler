tryToEdit : Int -> Result Error TUnit -> Msg
tryToEdit index result =
    case result of
        Ok _ ->
            GotSelectedMsg index StartEditingText

        Err err ->
            LogError (domErrToString err)
