type Maybe a = Just a | Nothing

type Result error value = Ok value | Err error

type List a = Nil | Cons a (List a)

type Bool = True | False

append____CBR_builtin : List a -> List a -> List a
append____CBR_builtin xs ys =
  case xs of
    [] -> ys
    hd :: tl -> hd :: tl ++ ys

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
