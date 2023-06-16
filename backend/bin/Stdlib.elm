type Maybe q = Just q | Nothing

type Result error value = Ok value | Err error

type List a = Nil | Cons a (List a)

type Bool = True | False

maybeMap____CBR : (a -> b) -> Maybe a -> Maybe b
maybeMap____CBR f mx =
  case mx of
    Just x -> Just (f x)
    Nothing -> Nothing

maybeWithDefault____CBR : a -> Maybe a -> a
maybeWithDefault____CBR d mx =
  case mx of
    Just x -> x
    Nothing -> d
