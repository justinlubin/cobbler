(type Bool
  (False Unit)
  (True Unit))

(type Peano
  (Zero Unit)
  (Succ Peano))

(type ListPeano
  (Nil Unit)
  (Cons (Peano * ListPeano)))

(define map : ((Peano -> Peano) -> (ListPeano -> ListPeano))
  (lambda f (Peano -> Peano) (lambda xs ListPeano
    (match xs
      (Nil n -> (Nil n))
      (Cons p -> (Cons ((f (fst p)) , (map f (snd p)))))))))

(define filter : ((Peano -> Bool) -> (ListPeano -> ListPeano))
  (lambda pred (Peano -> Bool) (lambda xs ListPeano
    (match xs
      (Nil n ->
        (Nil n))
      (Cons p ->
        (match (pred (fst p))
          (False n -> (filter pred (snd p)))
          (True n -> (Cons ((fst p) , (filter pred (snd p)))))))))))

(define main :
  ((Peano -> Bool) -> ((Peano -> Peano) -> (ListPeano -> ListPeano)))
  (lambda pred (Peano -> Bool) (lambda f (Peano -> Peano) (lambda xs ListPeano
    (match xs
      (Nil n ->
        (Nil n))
      (Cons p ->
        (match (pred (fst p))
          (False n -> (main pred f (snd p)))
          (True n -> (Cons ((f (fst p)) , (main pred f (snd p))))))))))))
