(type Bool
  (False Unit)
  (True Unit))

(type Peano
  (Zero Unit)
  (Succ Peano))

(type PeanoList
  (Nil Unit)
  (Cons (Peano * PeanoList)))

(define map : ((Peano -> Peano) -> (PeanoList -> PeanoList))
  (lambda f (Peano -> Peano) (lambda xs PeanoList
    (match xs
      (Nil n -> (Nil n))
      (Cons p -> (Cons ((f (fst p)) , (map f (snd p)))))))))

(define filter : ((Peano -> Bool) -> (PeanoList -> PeanoList))
  (lambda pred (Peano -> Bool) (lambda xs PeanoList
    (match xs
      (Nil n -> (Nil n))
      (Cons p ->
        (match (pred (fst p))
          (False n -> (filter p (snd p)))
          (True n -> (Cons ((fst p), (filter p (snd p)))))))))))

(define main : ((Peano -> Peano) -> (MaybePeano -> Peano))
  (lambda f (Peano -> Peano) (lambda mx MaybePeano
    (match mx
      (Nothing n -> zero)
      (Just x -> (f x))))))
