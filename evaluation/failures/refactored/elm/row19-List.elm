getColumnHelper : Int -> Array a -> List (Array a) -> Maybe (Array a)
getColumnHelper column items data =
    case data of
        head :: rest ->
            case Array.get column head of
                Just item ->
                    getColumnHelper column (Array.push item items) rest

                Nothing ->
                    Nothing

        [] ->
            Just items

-- *** TODO