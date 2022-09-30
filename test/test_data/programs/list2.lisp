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

(define mapmap : ((Peano -> Peano) -> ((Peano -> Peano) -> (ListPeano -> ListPeano)))
  (lambda f (Peano -> Peano) (lambda g (Peano -> Peano) (lambda xs ListPeano
    (map f (map g xs))))))

(define mapfilter : ((Peano -> Peano) -> ((Peano -> Bool) -> (ListPeano -> ListPeano)))
  (lambda f (Peano -> Peano) (lambda pred (Peano -> Bool) (lambda xs ListPeano
    (map f (filter pred xs))))))

(define main :
  ((Peano -> Peano) -> ((Peano -> Peano) -> (ListPeano -> ListPeano)))
  (lambda f (Peano -> Bool) (lambda g (Peano -> Peano) (lambda xs ListPeano
    (match xs
      (Nil n -> (Nil n))
      (Cons p -> (Cons ((f (g (fst p))) , (main f g (snd p))))))))))
