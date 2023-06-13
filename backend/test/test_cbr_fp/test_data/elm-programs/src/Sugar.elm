module Sugar exposing (..)

map : (a -> b) -> List a -> List b
map f xs =
    case xs of
        [] ->
            []

        hd :: tl ->
            f hd :: map f tl


filter : (a -> Bool) -> List a -> List a
filter pred xs =
    case xs of
        [] ->
            []

        hd :: tl ->
            if pred hd then
                filter pred tl

            else
                hd :: filter pred tl


target : (a -> Bool) -> (a -> b) -> List a -> List b
target pred f xs =
    case xs of
        [] ->
            []

        hd :: tl ->
            if pred hd then
                target pred f tl

            else
                f hd :: target pred f tl
