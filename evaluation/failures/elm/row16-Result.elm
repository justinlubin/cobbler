map2 : (a -> b -> value) -> Result x a -> Result x b -> Result x value
map2 func ra rb =
    case ra of
        Err x ->
            Err x

        Ok a ->
            case rb of
                Err x ->
                    Err x

                Ok b ->
                    Ok (func a b)
