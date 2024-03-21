listGeneral : (any -> acc -> acc) -> acc -> List any -> acc
listGeneral f acc rest =
    case rest of
        [] ->
            acc

        current :: newRest ->
            listGeneral f (f current acc) newRest