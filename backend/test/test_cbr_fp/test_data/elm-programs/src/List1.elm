module List1 exposing (..)


type B
    = Fal
    | Tru


type L a
    = Nil
    | Cons a (L a)


map : (a -> b) -> L a -> L b
map f xs =
    case xs of
        Nil ->
            Nil

        Cons hd tl ->
            Cons (f hd) (map f tl)


filter : (a -> B) -> L a -> L a
filter pred xs =
    case xs of
        Nil ->
            Nil

        Cons hd tl ->
            case pred hd of
                Fal ->
                    filter pred tl

                Tru ->
                    Cons hd (filter pred tl)


target : (a -> B) -> (a -> b) -> L a -> L b
target pred f xs =
    case xs of
        Nil ->
            Nil

        Cons hd tl ->
            case pred hd of
                Fal ->
                    target pred f tl

                Tru ->
                    Cons (f hd) (target pred f tl)
