( () 
    ((Assign x (Call + (Hole a) (Hole b)))
    (Assign y (Call - (Call + (Num 2) (Num 1)) (Num 3)))
    (Assign z (Call * y x))))