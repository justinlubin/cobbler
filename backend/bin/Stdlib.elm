type Maybe a = Just a | Nothing

type Result error value = Ok value | Err error

type List a = Nil | Cons a (List a)

type Bool = True | False

append____CBR_builtin : List a -> List a -> List a
append____CBR_builtin xs ys =
  case xs of
    [] -> ys
    hd :: tl -> hd :: tl ++ ys

or____CBR_inline : Bool -> Bool -> Bool
or____CBR_inline p q =
  case p of
    True -> True
    False -> q

and____CBR_inline : Bool -> Bool -> Bool
and____CBR_inline p q =
  case p of
    True -> q
    False -> False

basics_not____CBR : Bool -> Bool
basics_not____CBR b =
  case b of
    True -> False
    False -> True

maybe_map____CBR : (a -> b) -> Maybe a -> Maybe b
maybe_map____CBR f mx =
  case mx of
    Just x -> Just (f x)
    Nothing -> Nothing

maybe_withDefault____CBR : a -> Maybe a -> a
maybe_withDefault____CBR d mx =
  case mx of
    Just x -> x
    Nothing -> d

result_map____CBR : (a -> value) -> Result x a -> Result x value
result_map____CBR f rx =
  case rx of
    Ok p -> Ok (f p)
    Err q -> Err q

result_mapError____CBR : (x -> y) -> Result x a -> Result y a
result_mapError____CBR f rx =
  case rx of
    Ok p -> Ok p
    Err q -> Err (f q)

result_withDefault____CBR : a -> Result x a -> a
result_withDefault____CBR d rx =
  case rx of
    Ok p -> p
    Err q -> d

list_map____CBR : (a -> b) -> List a -> List b
list_map____CBR f xs =
  case xs of
    [] -> []
    hd :: tl -> f hd :: list_map____CBR f tl

list_filter____CBR : (a -> Bool) -> List a -> List a
list_filter____CBR p xs =
  case xs of
    [] -> []
    hd :: tl ->
      if p hd then hd :: list_filter____CBR p tl else list_filter____CBR p tl

list_concat____CBR : List (List a) -> List a
list_concat____CBR xss =
  case xss of
    [] -> []
    hd :: tl ->
      hd ++ list_concat____CBR tl

list_find____CBR : (a -> Bool) -> List a -> Maybe b
list_find____CBR p xs =
  case xs of
    [] -> Nothing
    hd :: tl ->
      case p hd of
        True -> Just hd
        False -> list_find____CBR p tl

list_findMap____CBR : (a -> Maybe b) -> List a -> Maybe b
list_findMap____CBR f xs =
  case xs of
    [] -> Nothing
    hd :: tl ->
      case f hd of
        Nothing -> list_findMap____CBR f tl
        Just y -> Just y

list_any____CBR : (a -> Bool) -> List a -> Bool
list_any____CBR p xs =
  case xs of
    [] -> False
    hd :: tl -> if p hd then True else list_any____CBR p tl

list_head____CBR : List a -> Maybe a
list_head____CBR xs =
  case xs of
    [] -> Nothing
    hd :: tl -> Just hd

list_tail____CBR : List a -> Maybe (List a)
list_tail____CBR xs =
  case xs of
    [] -> Nothing
    hd :: tl -> Just tl

-- Catamorphisms and other destructors
--
-- maybe_catamorphism____CBR : b -> (a -> b) -> Maybe a -> b
-- maybe_catamorphism____CBR base f mx =
--   case mx of
--     Nothing -> base
--     Just x -> f x
--
-- result_catamorphism____CBR : (a -> c) -> (b -> c) -> Result a b -> c
-- result_catamorphism____CBR f g rx =
--   case rx of
--     Err x -> f x
--     Ok y -> g y
--
-- list_catamorphism____CBR : b -> (a -> b -> b) -> List a -> b
-- list_catamorphism____CBR acc f xs =
--   case xs of
--     [] -> acc
--     hd :: tl -> f hd (list_catamorphism____CBR acc f tl)
--
-- list_uncons____CBR : b -> (a -> List a -> b) -> List a -> b
-- list_uncons____CBR nil cons xs =
--   case xs of
--     [] -> nil
--     hd :: tl -> cons hd tl
