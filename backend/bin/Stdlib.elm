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
