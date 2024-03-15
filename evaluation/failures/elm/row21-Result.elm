unwrapResult : (err -> ok) -> Result err ok -> ok
unwrapResult fromError result =
    case result of
        Ok value ->
            value

        Err error ->
            fromError error
