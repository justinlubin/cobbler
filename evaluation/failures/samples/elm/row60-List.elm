chooseByBoolList : List Bool -> List a -> List a
chooseByBoolList boolList list =
    case boolList of
        boolHead :: boolTail ->
            case list of
                head :: tail ->
                    case boolHead of
                        True ->
                            head :: chooseByBoolList boolTail tail

                        False ->
                            chooseByBoolList boolTail tail

                [] ->
                    []

        [] ->
            []
