neighbors : Set Position -> List Position -> Set Position -> Set Position
neighbors walls positions result =
    case positions of
        [] ->
            result

        pos :: xs ->
            case Set.member pos walls of
                True ->
                    neighbors walls xs result

                False ->
                    neighbors walls xs (Set.insert pos result)
