(define zero
  (Zero (lambda z z)))

(define map
  (lambda f (lambda mx
    (match mx
      (Nothing n -> (Nothing n))
      (Just x -> (Just (f x)))))))

(define withDefault
  (lambda default (lambda mx
    (match mx
      (Nothing n -> default)
      (Just x -> x)))))

(define main
  (lambda f (lambda mx
    (match mx
      (Nothing n -> zero)
      (Just x -> (f x))))))
