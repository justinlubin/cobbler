type Maybe a = Just a | Nothing

type Result error value = Ok value | Err error

type List a = Nil | Cons a (List a)