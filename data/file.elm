main : (String -> Bool) ->  Maybe String -> Maybe Bool
main f mx =
    case mx of
        Just x ->
          f x

        Nothing ->
            False
