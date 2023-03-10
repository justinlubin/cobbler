(((dot (x y) 
         ((Assign out (Call zeros (Call len x))) 
         (For i (Call range (Call len x)) 
                ((Assign (Index out i) (Call + (Index out i) (Call *
   (Index x i) (Index y i))))
          ))
	 (Return out)))
         (sum (x) 
           ((Assign out (Num 0))\n\
           (For i (Call range (Call len x))\n\
                  ((Assign out (Call add out (Index x i)))))
	   (Return out)))
        (mul (x y)
           ((Assign out (Call zeros (Call len x)))\n\
            (For i (Call range (Call len x))\n\
               ((Assign (Index out i) (Call mul (Index x i) (Index y i)))))
	    (Return out)
           ))) () )
