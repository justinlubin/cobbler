module Main exposing (decodeLoaded)


decodeLoaded : Json.Decode.Decoder a -> Maybe.Maybe String -> LoadResult a
decodeLoaded decoder mstring =
    case mstring of
        Maybe.Just string ->
          f string

        Maybe.Nothing ->
            NoData
