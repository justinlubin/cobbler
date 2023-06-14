(
    ((rolling_cg_pair_sum (N seq)(
        (Assign cp (Call np_concatenate (List (Call & (Call == (Slice seq 0 -1) "C") (Call == (Slice seq 1 end) "G")) (List False)) ))
        (Assign kernel (Call np_ones N))
        (Assign (Index kernel 0) 0)
        (Return (Call np_convolve cg kernel "valid"))
    ))
    )
    ()
)