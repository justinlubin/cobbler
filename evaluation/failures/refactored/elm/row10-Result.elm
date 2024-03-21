getField : (Date -> a) -> Result String Date -> String
getField fn date =
    case date of
        Ok value ->
            toString (fn value)

        Err msg ->
            msg

-- *** Result.catamorphism (auto)

getField : (Date -> a) -> Result String Date -> String
getField fn date =
    date
        |> Result.catamorphism (\y -> y) (\x -> toString (fn x))