module Main exposing (decodeLoaded)


decodeLoaded : Json.Decode.Decoder a -> Maybe.Maybe String -> LoadResult a
decodeLoaded decoder mstring =
    case mstring of
        Maybe.Just string ->
            case Json.Decode.decodeString decoder string of
                Ok data ->
                    Data data

                Err err ->
                    Error err

        Maybe.Nothing ->
            NoData
