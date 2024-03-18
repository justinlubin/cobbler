applicationFromListSM : PatternSansMeta -> List PatternSansMeta -> PatternSansMeta
applicationFromListSM acc li =
    case li of
        [] ->
            acc

        right :: rest ->
            applicationFromListSM (PApplicationSM acc right) rest
