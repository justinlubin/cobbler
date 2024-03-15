attempted : Result err val -> Type
attempted act =
    case act of
        Ok _ ->
            None

        Err msg ->
            Failed (Struct.Error.new Failure (toString msg))
