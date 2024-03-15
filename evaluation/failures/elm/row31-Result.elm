mapBoth : (e -> b) -> (a -> b) -> Result e a -> b
mapBoth fromError fromOK result =
    case result of
        Err err ->
            fromError err

        Ok ok ->
            fromOK ok
