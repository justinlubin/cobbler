open Cbr_numpy.Lang
open Cbr_numpy.Inline
open Core

let sum_body = 
  [ Assign (Name "count", Call (Name "+", [Name "count"; Name "xi"]))]
let sum_defn = 
  ["x1"], 
  [ Assign (Name "count", Num 0) 
  ; For ("xi", Name "x1", sum_body) 
  ; Return (Name "count")
  ]

let env : env = Map.of_alist_exn (module String) ["sum", sum_defn]

let p1 = 
  env, 
  [ Return (Call (Name "sum", [Call (Name "add", [Name "x"; Name "y"])]))]

let p2 = 
  Map.empty (module String),
  [ Return (Call (Name "sum", [Call (Name "add", [Name "x"; Name "y"])]))]

let inlined1 = 
  env, 
  [ Assign (Name "count", Num 0)
  ; For ("xi", Call (Name "add", [Name "x"; Name "y"]), sum_body)
  ; Return (Name "count")
  ]

let%test_unit "inline 1" =
  [%test_result: program]
    (inline_program p1)
    ~expect:inlined1

let%test_unit "empty env inline" =
  [%test_result: program]
    (inline_program p2)
    ~expect:p2



