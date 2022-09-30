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

(define mapmap : ((Peano -> Peano) -> ((Peano -> Peano) -> (ListPeano -> ListPeano)))
  (lambda f (Peano -> Peano) (lambda g (Peano -> Peano) (lambda xs ListPeano
    (map f (map g xs))))))

(define main :
  ((Peano -> Peano) -> ((Peano -> Peano) -> (ListPeano -> ListPeano)))
  (lambda f (Peano -> Bool) (lambda g (Peano -> Peano) (lambda xs ListPeano
    (match xs
      (Nil n -> (Nil n))
      (Cons p -> (Cons ((f (g (fst p))) , (main f g (snd p))))))))))
