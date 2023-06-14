(type (Peano)
  (Zero)
  (Succ (Peano)))

(type (LongList a)
  (LongNil)
  (LongCons a (LongList a)))

(define main :
  (((Peano) -> (Peano)) -> ((LongList (Peano)) -> (LongList (Peano))))
  (lambda f (lambda xs
    (match xs
      ((LongNil) ->
        (LongNil))
      ((LongCons hd tl) ->
        (LongCons (f hd) (main f tl)))))))
