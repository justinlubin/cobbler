shareStatusFromHttpResult : Result Error String -> ShareStatus
shareStatusFromHttpResult result =
    case result of
        Ok gameKey ->
            ShareExists gameKey

        Err error ->
            ShareFailed error
