(define zero : Peano
  (Zero ()))

(define map : ((Peano -> Peano) -> (MaybePeano -> MaybePeano))
  (lambda f (Peano -> Peano) (lambda mx MaybePeano
    (match mx
      (Nothing n -> (Nothing n))
      (Just x -> (Just (f x)))))))

(define withDefault : (Peano -> (MaybePeano -> Peano))
  (lambda default Peano (lambda mx MaybePeano
    (match mx
      (Nothing n -> default)
      (Just x -> x)))))

(define main : ((Peano -> Peano) -> (MaybePeano -> Peano))
  (lambda f (Peano -> Peano) (lambda mx MaybePeano
    (match mx
      (Nothing n -> zero)
      (Just x -> (f x))))))
