(type (Bool)
  (False)
  (True))

(type (Peano)
  (Zero)
  (Succ (Peano)))

(type (ListPeano)
  (Nil)
  (Cons (Peano) (ListPeano)))

(define map : (((Peano) -> (Peano)) -> ((ListPeano) -> (ListPeano)))
  (lambda f ((Peano) -> (Peano)) (lambda xs (ListPeano)
    (match xs
      ((Nil) -> (Nil))
      ((Cons hd tl) -> (Cons (f hd) (map f tl)))))))

(define filter : (((Peano) -> (Bool)) -> ((ListPeano) -> (ListPeano)))
  (lambda pred ((Peano) -> (Bool)) (lambda xs (ListPeano)
    (match xs
      ((Nil) ->
        (Nil))
      ((Cons hd tl) ->
        (match (pred hd)
          ((False) -> (filter pred tl))
          ((True) -> (Cons hd (filter pred tl)))))))))

(define main :
  (((Peano) -> (Bool)) -> (((Peano) -> (Peano)) -> ((ListPeano) -> (ListPeano))))
  (lambda pred ((Peano) -> (Bool)) (lambda f ((Peano) -> (Peano)) (lambda xs (ListPeano)
    (match xs
      ((Nil) ->
        (Nil))
      ((Cons hd tl) ->
        (match (pred hd)
          ((False) -> (main pred f tl))
          ((True) -> (Cons (f hd) (main pred f tl))))))))))
