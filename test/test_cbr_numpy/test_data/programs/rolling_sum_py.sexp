(((rolling_cg_pair_sum (N seq) (
    (Assign moving_sum (Call np_zeros (Call + (Call - (Call len seq) N) 1) ))
    (For start (Call range (Call len moving_sum))(
        (For i (Call range (Call - N 1)) (
            (Assign (Index moving_sum start) (Call + (Index moving_sum start) 1))))
    )
    ))
    ))
    ()
)