(((matmul (x y) (
    (Assign out (Call zeros (Call len x) (Call len (Index y (Num 0)))))
    (For i (Call range (Call len x)) (
        (For j (Call range (Call len (Index y (Num 0))))(
            (Assign dot (Num 0))
            (For k (Call range (Call len y)) (
                (Assign dot (Call + dot (Call * (Index (Index x i) k) (Index (Index y k) j))))

            ))
            (Assign (Index (Index out i) j) dot))
        )))
    (Return out))
))
(
    (Assign x (Call zeros (Num 2) (Num 2)))
    (Assign y (Call zeros (Num 2) (Num 2)))
    (Assign i (Num 1))
    (Assign (Index (Index x (Num 0)) (Num 0)) (Num 3))
    (Assign (Index (Index y (Num 1)) (Num 0)) (Num 5))
    (Assign (Index (Index x (Call - i (Num 1))) (Call * i (Num 1))) (Num 4))
    (Assign z (Call matmul x y))
))