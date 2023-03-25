open Lang
open Core

let sum_body =
  [ Assign (Name "sum_count", Call (Name "+", [ Name "sum_count"; Name "sum_i" ])) ]

let sum_defn =
  ( [ "sum_1" ]
  , [ Assign (Name "sum_count", Num 0)
    ; For ("sum_i", Name "sum_1", sum_body)
    ; Return (Name "sum_count")
    ] )

let np_env : env =
  String.Map.of_alist_exn [ ("sum", sum_defn) ]