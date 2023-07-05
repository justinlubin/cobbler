titleForMe : Maybe.Maybe Viewer.Cred.Cred -> Username.Username -> String
titleForMe maybeCred username =
    case maybeCred of
        Maybe.Just cred ->
            case (==) username (Viewer.Cred.username cred) of
                Basics.True ->
                    myProfileTitle

                Basics.False ->
                    defaultTitle

        Maybe.Nothing ->
            defaultTitle
