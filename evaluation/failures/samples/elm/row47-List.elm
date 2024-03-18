buildDict : Dict String Int -> List String -> Dict String Int
buildDict d lines =
    case lines of
        x :: xs ->
            case not (Dict.member x d) of
                True ->
                    buildDict (Dict.insert x (countElemOccurance x lines) d) xs

                False ->
                    buildDict d xs

        [] ->
            d
