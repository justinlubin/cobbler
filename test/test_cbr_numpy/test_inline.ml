open Cbr_numpy.Lang
open Cbr_numpy.Inline
open Core

let sum_body =
  [ Assign (Name "count", Call (Name "+", [ Name "count"; Name "xi" ])) ]

let sum_defn =
  ( [ "x1" ]
  , [ Assign (Name "count", Num 0)
    ; For ("xi", Name "x1", sum_body)
    ; Return (Name "count")
    ] )

let f_defn = ([ "x" ], [ Assign (Name "x", Str "cat"); Return (Name "x") ])
let id_defn = ([ "x" ], [ Return (Name "x") ])

let env : env =
  Map.of_alist_exn
    (module String)
    [ ("sum", sum_defn); ("f", f_defn); ("id", id_defn) ]

let p1 =
  ( env
  , [ Return (Call (Name "sum", [ Call (Name "add", [ Name "x"; Name "y" ]) ]))
    ] )

let p2 =
  ( String.Map.empty
  , [ Return (Call (Name "sum", [ Call (Name "add", [ Name "x"; Name "y" ]) ]))
    ] )

let p3 = (env, [ Assign (Name "x", Num 5); Return (Call (Name "f", [ Num 0 ])) ])

let p4 =
  (env, [ Assign (Name "x", Num 5); Return (Call (Name "id", [ Num 0 ])) ])

let inline_of_p1 =
  ( env
  , [ Assign (Name "count", Num 0)
    ; For ("xi", Call (Name "add", [ Name "x"; Name "y" ]), sum_body)
    ; Return (Name "count")
    ] )

let inline_of_p3 =
  ( env
  , [ Assign (Name "x", Num 5)
    ; Assign (Name "x", Str "cat")
    ; Return (Name "x")
    ] )

let inline_of_p4 = (env, [ Assign (Name "x", Num 5); Return (Num 0) ])

let%test_unit "inline 1" =
  [%test_result: program] (inline_program p1) ~expect:inline_of_p1

let%test_unit "empty env inline" =
  [%test_result: program] (inline_program p2) ~expect:p2

let%test_unit "inline with name conflict" =
  [%test_result: program] (inline_program p3) ~expect:inline_of_p3

let%test_unit "inline 4" =
  [%test_result: program] (inline_program p4) ~expect:inline_of_p4
