listGeneral : (any -> acc -> acc) -> acc -> List any -> acc
listGeneral f acc rest =
    case rest of
        [] ->
            acc

        current :: newRest ->
            listGeneral f (f current acc) newRest

-- *** List.foldl (exact)

listGeneral : (any -> acc -> acc) -> acc -> List any -> acc
listGeneral f acc rest =
    List.foldl f acc rest