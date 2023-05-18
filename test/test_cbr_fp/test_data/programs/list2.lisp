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

(define mapmap : (((Peano) -> (Peano)) -> (((Peano) -> (Peano)) -> ((ListPeano) -> (ListPeano))))
  (lambda f ((Peano) -> (Peano)) (lambda g ((Peano) -> (Peano)) (lambda xs (ListPeano)
    (map f (map g xs))))))

(define mapfilter : (((Peano) -> (Peano)) -> (((Peano) -> (Bool)) -> ((ListPeano) -> (ListPeano))))
  (lambda f ((Peano) -> (Peano)) (lambda pred ((Peano) -> (Bool)) (lambda xs (ListPeano)
    (map f (filter pred xs))))))

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
