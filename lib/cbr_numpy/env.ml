open Lang
open Core

let sum_body =
  [ Assign
      (PHole (Number, "sum_count"), Call (Name "+", [ Hole (Number, "sum_count"); Hole (Number, "sum_i") ]))
  ]

let sum_defn =
  ( [ "sum_1" ]
  , [ Assign (PHole (Number, "sum_count"), Num 0)
    ; For (PHole (Number, "sum_i"), Name "sum_1", sum_body)
    ; Return (Hole (Number, "sum_count"))
    ] )

let np_env : env = String.Map.of_alist_exn [ ("sum", sum_defn) ]
