(define zero : Peano
  (Zero (lambda z z)))

(define map : ((Peano -> Peano) -> (MaybePeano -> MaybePeano))
  (lambda f (lambda mx
    (match mx
      (Nothing n -> (Nothing n))
      (Just x -> (Just (f x)))))))

(define withDefault : (Peano -> (MaybePeano -> Peano))
  (lambda default (lambda mx
    (match mx
      (Nothing n -> default)
      (Just x -> x)))))

(define main : ((Peano -> Peano) -> (MaybePeano -> Peano))
  (lambda f (lambda mx
    (match mx
      (Nothing n -> zero)
      (Just x -> (f x))))))
