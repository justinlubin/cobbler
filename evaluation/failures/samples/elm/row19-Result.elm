viewLoadResult : Result String MidiRecording -> String
viewLoadResult mr =
    case mr of
        Ok res ->
            "OK: " ++ toString res

        Err errs ->
            "Fail: " ++ errs
