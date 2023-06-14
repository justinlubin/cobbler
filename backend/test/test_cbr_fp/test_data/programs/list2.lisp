(type (Bool)
  (False)
  (True))

(type (Peano)
  (Zero)
  (Succ (Peano)))

(type (List a)
  (Nil)
  (Cons a (List a)))

(define map : (((Peano) -> (Peano)) -> ((List (Peano)) -> (List (Peano))))
  (lambda f (lambda xs
    (match xs
      ((Nil) -> (Nil))
      ((Cons hd tl) -> (Cons (f hd) (map f tl)))))))

(define filter : (((Peano) -> (Bool)) -> ((List (Peano)) -> (List (Peano))))
  (lambda pred (lambda xs
    (match xs
      ((Nil) ->
        (Nil))
      ((Cons hd tl) ->
        (match (pred hd)
          ((False) -> (filter pred tl))
          ((True) -> (Cons hd (filter pred tl)))))))))

(define mapmap : (((Peano) -> (Peano)) -> (((Peano) -> (Peano)) -> ((List (Peano)) -> (List (Peano)))))
  (lambda f (lambda g (lambda xs
    (map f (map g xs))))))

(define mapfilter : (((Peano) -> (Peano)) -> (((Peano) -> (Bool)) -> ((List (Peano)) -> (List (Peano)))))
  (lambda f (lambda pred (lambda xs
    (map f (filter pred xs))))))

(define main :
  (((Peano) -> (Bool)) -> (((Peano) -> (Peano)) -> ((List (Peano)) -> (List (Peano)))))
  (lambda pred (lambda f (lambda xs
    (match xs
      ((Nil) ->
        (Nil))
      ((Cons hd tl) ->
        (match (pred hd)
          ((False) -> (main pred f tl))
          ((True) -> (Cons (f hd) (main pred f tl))))))))))
