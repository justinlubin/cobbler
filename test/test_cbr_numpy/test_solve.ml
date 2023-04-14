open Cbr_numpy
open Core
open Np_synthesis
open Lang
open Parse

let target1 : program =
  ( Cbr_numpy.Env.np_env
  , [ Assign (PName "sum_count", Num 0)
    ; For
        ( PName "sum_i"
        , Name "x"
        , [ Assign
              ( PName "sum_count"
              , Call (Name "+", [ Name "sum_count"; Name "sum_i" ]) )
          ] )
    ; Return (Name "sum_count")
    ] )

let solution1 : program =
  (Cbr_numpy.Env.np_env, [ Return (Call (Name "sum", [ Name "x" ])) ])

let target2 : program =
  ( Cbr_numpy.Env.np_env
  , [ Assign (PName "x", Num 0); Return (Call (Name "+", [ Name "x"; Num 1 ])) ]
  )

let%test_unit "np_solve 1" =
  [%test_result: program]
    (match solve 1 target1 with
    | Some p -> p
    | None -> failwith "no solution")
    ~expect:solution1

let%test_unit "np_solve no solution" =
  [%test_result: program option] (solve 3 target2) ~expect:None
