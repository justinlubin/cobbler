(type (Bool)
  (False)
  (True))

(type (Peano)
  (Zero)
  (Succ (Peano)))

(type (List a)
  (Nil)
  (Cons a (List a)))

(define map : ((a -> a) -> ((List a) -> (List a)))
  (lambda f (lambda xs
    (match xs
      ((Nil) -> (Nil))
      ((Cons hd tl) -> (Cons (f hd) (map f tl)))))))

(define filter : ((a -> (Bool)) -> ((List a) -> (List a)))
  (lambda pred (lambda xs
    (match xs
      ((Nil) ->
        (Nil))
      ((Cons hd tl) ->
        (match (pred hd)
          ((False) -> (filter pred tl))
          ((True) -> (Cons hd (filter pred tl)))))))))

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
