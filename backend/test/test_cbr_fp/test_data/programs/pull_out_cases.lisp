(type (Maybe a)
  (Nothing)
  (Just a))

(type (Peano)
  (Zero)
  (Succ (Peano)))

(define main : (((Peano) -> (Peano)) -> ((Maybe (Peano)) -> (Peano)))
  (lambda f (lambda mx
    (match (match mx ((Nothing) -> (Nothing)) ((Just x) -> (Just (f x))))
      ((Nothing) -> (Zero))
      ((Just y) -> y)))))
