(type (Peano)
  (Zero)
  (Succ (Peano)))

(type (Maybe a)
  (Nothing)
  (Just a))

(define zero : (Peano)
  (Zero))

(define map : (((Peano) -> (Peano)) -> ((Maybe (Peano)) -> (Maybe (Peano))))
  (lambda f ((Peano) -> (Peano)) (lambda mx (Maybe (Peano))
    (match mx
      ((Nothing) -> (Nothing))
      ((Just x) -> (Just (f x)))))))

(define withDefault : ((Peano) -> ((Maybe (Peano)) -> (Peano)))
  (lambda default (Peano) (lambda mx (Maybe (Peano))
    (match mx
      ((Nothing) -> default)
      ((Just x) -> x)))))

(define main : (((Peano) -> (Peano)) -> ((Maybe (Peano)) -> (Peano)))
  (lambda f ((Peano) -> (Peano)) (lambda mx (Maybe (Peano))
    (withDefault zero (map f mx)))))
