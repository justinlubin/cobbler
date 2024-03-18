failOrProceed : Result Errors Msg -> Cmd Msg
failOrProceed msgResult =
    case msgResult of
        Ok msg ->
            Task.perform (always msg) (Process.sleep 0)

        Err error ->
            buildFailed (Json.Encode.list Morphir.Elm.IncrementalFrontend.Codec.encodeError error)
