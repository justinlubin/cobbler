type Maybe a = Just a | Nothing

type Result error value = Ok value | Err error

type List a = Nil | Cons a (List a)

type Bool = True | False

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
