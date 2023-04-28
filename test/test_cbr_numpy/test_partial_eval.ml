open Cbr_numpy
open Lang
open Partial_eval
open Parse
open Core

let env = Map.empty (module String)

let p1 =
  ( env
  , [ Return (Call (Name "len", [ Call (Name "mul", [ Name "x"; Name "y" ]) ]))
    ] )

let expect1 = (env, [ Return (Call (Name "len", [ Name "x" ])) ])

let p2 =
  ( env
  , [ Assign
        ( PIndex (PName "a", Num 1)
        , Index (Call (Name "mul", [ Name "x"; Name "y" ]), Num 0) )
    ; Return (Name "a")
    ] )

let expect2 =
  ( env
  , [ Assign
        ( PIndex (PName "a", Num 1)
        , Call (Name "*", [ Index (Name "x", Num 0); Index (Name "y", Num 0) ])
        )
    ; Return (Name "a")
    ] )

let%test_unit "inline 1" =
  [%test_result: program] (partial_eval_program p1) ~expect:expect1

let%test_unit "empty env inline" =
  [%test_result: program] (partial_eval_program p2) ~expect:expect2
