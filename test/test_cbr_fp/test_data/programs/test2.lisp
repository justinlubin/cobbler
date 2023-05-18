(type (Peano)
  (Zero Unit)
  (Succ (Peano)))

(type (Maybe a)
  (Nothing Unit)
  (Just a))

(define zero : (Peano)
  (Zero ()))

(define map : (((Peano) -> (Peano)) -> ((Maybe (Peano)) -> (Maybe (Peano))))
  (lambda f ((Peano) -> (Peano)) (lambda mx (Maybe (Peano))
    (match mx
      ((Nothing n) -> (Nothing n))
      ((Just x) -> (Just (f x)))))))

(define withDefault : ((Peano) -> ((Maybe (Peano)) -> (Peano)))
  (lambda default (Peano) (lambda mx (Maybe (Peano))
    (match mx
      ((Nothing n) -> default)
      ((Just x) -> x)))))

(define main : (((Peano) -> (Peano)) -> ((Maybe (Peano)) -> (Peano)))
  (lambda f ((Peano) -> (Peano)) (lambda mx (Maybe (Peano))
    (withDefault zero (map f mx)))))
