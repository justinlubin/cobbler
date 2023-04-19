open Lang
open Core

let sum_body =
  [ Assign
      ( PHole (Number, "sum_count")
      , Call (Name "+", [ Hole (Number, "sum_count"); Index (Name "sum_1", Hole (Number, "sum_i")) ])
      )
  ]

let sum_defn =
  ( [ "sum_1" ]
  , [ Assign (PHole (Number, "sum_count"), Num 0)
    ; For (PHole (Number, "sum_i"), Call (Name "range", [Call (Name "len", [Name "sum_1"])]), sum_body)
    ; Return (Hole (Number, "sum_count"))
    ] )

let mul_body =
  [ Assign
      ( PIndex (PHole (Array, "mul_result"), Hole (Number, "mul_i"))
      , Call
          ( Name "*"
          , [ Index (Name "mul_1", Hole (Number, "mul_i"))
            ; Index (Name "mul_2", Hole (Number, "mul_i"))
            ] ) )
  ]

let mul_defn =
  ( [ "mul_1"; "mul_2" ]
  , [ Assign
        ( PHole (Array, "mul_result")
        , Call (Name "zeros", [ Call (Name "len", [ Name "mul_1" ]) ]) )
    ; For
        ( PHole (Number, "mul_i")
        , Call (Name "range", [ Call (Name "len", [ Name "mul_1" ]) ])
        , mul_body )
    ; Return (Hole (Array, "mul_result"))
    ] )

let add_body =
  [ Assign
      ( PIndex (PHole (Array, "add_result"), Hole (Number, "add_i"))
      , Call
          ( Name "*"
          , [ Index (Name "add_1", Hole (Number, "add_i"))
            ; Index (Name "add_2", Hole (Number, "add_i"))
            ] ) )
  ]

let add_defn =
  ( [ "add_1"; "add_2" ]
  , [ Assign
        ( PHole (Array, "add_result")
        , Call (Name "zeros", [ Call (Name "len", [ Name "add_1" ]) ]) )
    ; For
        ( PHole (Number, "add_i")
        , Call (Name "range", [ Call (Name "len", [ Name "add_1" ]) ])
        , add_body )
    ; Return (Hole (Array, "add_result"))
    ] )

let np_env : env =
  String.Map.of_alist_exn [ ("sum", sum_defn); ("mul", mul_defn); ("add", add_defn) ]
