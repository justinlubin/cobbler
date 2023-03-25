open Cbr_numpy
open Np_synthesis
open Lang
open Parse

let target1 : program =
  Cbr_numpy.Env.np_env,  
  [ Assign (Name "sum_count", Num 0)
  ; For ("sum_i", Name "x", 
    [ Assign (Name "sum_count", Call (Name "+", [Name "sum_count"; Name "sum_i"]))]
    )
  ; Return (Name "sum_count")
  ]

let solution1 : program =
  Cbr_numpy.Env.np_env,
  [ Return (Call (Name "sum", [Name "x"])) ]

let%test_unit "np_solve 1" =
  [%test_result: program] 
  ( match solve 1 target1 with 
    | Some p -> p
    | None -> failwith "no solution")
  ~expect:solution1
