getField : (Date -> a) -> Result String Date -> String
getField fn date =
    case date of
        Ok value ->
            toString (fn value)

        Err msg ->
            msg
