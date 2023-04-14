open Cbr_numpy
open Core
open Np_synthesis
open Lang
open Parse

let target1 : program =
  ( Cbr_numpy.Env.np_env
  , [ Assign (PName "y", Num 0)
    ; For
        ( PName "i"
        , Name "x"
        , [ Assign
              ( PName "y"
              , Call (Name "+", [ Name "y"; Name "i" ]) )
          ] )
    ; Return (Name "y")
    ] )

let solution1 : program =
  (Cbr_numpy.Env.np_env, [ Return (Call (Name "sum", [ Name "x" ])) ])

let no_sol_target : program =
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
  [%test_result: program option] (solve 1 no_sol_target) ~expect:None
