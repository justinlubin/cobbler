(type (Peano)
  (Zero)
  (Succ (Peano)))

(type (MaybePeano)
  (Nothing)
  (Just (Peano)))

(define zero : (Peano)
  (Zero))

(define map : (((Peano) -> (Peano)) -> ((MaybePeano) -> (MaybePeano)))
  (lambda f ((Peano) -> (Peano)) (lambda mx (MaybePeano)
    (match mx
      ((Nothing) -> (Nothing))
      ((Just x) -> (Just (f x)))))))

(define withDefault : ((Peano) -> ((MaybePeano) -> (Peano)))
  (lambda default (Peano) (lambda mx (MaybePeano)
    (match mx
      ((Nothing) -> default)
      ((Just x) -> x)))))

(define main : (((Peano) -> (Peano)) -> ((MaybePeano) -> (Peano)))
  (lambda f ((Peano) -> (Peano)) (lambda mx (MaybePeano)
    (match mx
      ((Nothing) -> zero)
      ((Just x) -> (f x))))))
