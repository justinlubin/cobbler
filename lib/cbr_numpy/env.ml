open Lang
open Core

let sum_body =
  [ Assign
      ( PHole (Number, "sum_count")
      , Call
          ( Name "+"
          , [ Hole (Number, "sum_count")
            ; Index (Name "sum_1", Hole (Number, "sum_i"))
            ] ) )
  ]

let sum_defn =
  ( [ "sum_1" ]
  , [ Assign (PHole (Number, "sum_count"), Num 0)
    ; For
        ( PHole (Number, "sum_i")
        , Call (Name "range", [ Call (Name "len", [ Name "sum_1" ]) ])
        , sum_body )
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
          ( Name "+"
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

let div_body =
  [ Assign
      ( PIndex (PHole (Array, "div_result"), Hole (Number, "div_i"))
      , Call
          ( Name "/"
          , [ Index (Name "div_1", Hole (Number, "div_i"))
            ; Index (Name "div_2", Hole (Number, "div_i"))
            ] ) )
  ]

let div_defn =
  ( [ "div_1"; "div_2" ]
  , [ Assign
        ( PHole (Array, "div_result")
        , Call (Name "zeros", [ Call (Name "len", [ Name "div_1" ]) ]) )
    ; For
        ( PHole (Number, "div_i")
        , Call (Name "range", [ Call (Name "len", [ Name "div_1" ]) ])
        , div_body )
    ; Return (Hole (Array, "div_result"))
    ] )

let ones_defn =
  ( [ "n" ]
  , [ Assign (PHole (Array, "ones_result"), Call (Name "zeros", [ Name "n" ]))
    ; For
        ( PHole (Number, "ones_i")
        , Call (Name "range", [ Name "n" ])
        , [ Assign
              ( PIndex (PHole (Array, "ones_result"), Hole (Number, "ones_i"))
              , Num 1 )
          ] )
    ; Return (Hole (Array, "ones_result"))
    ] )

let eq_body =
  [ Assign
      ( PIndex (PHole (Array, "eq_result"), Hole (Number, "eq_i"))
      , Call
          ( Name "=="
          , [ Index (Name "eq_1", Hole (Number, "eq_i"))
            ; Index (Name "eq_2", Hole (Number, "eq_i"))
            ] ) )
  ]

let eq_defn =
  ( [ "eq_1"; "eq_2" ]
  , [ Assign
        ( PHole (Array, "eq_result")
        , Call (Name "zeros", [ Call (Name "len", [ Name "eq_1" ]) ]) )
    ; For
        ( PHole (Number, "eq_i")
        , Call (Name "range", [ Call (Name "len", [ Name "eq_1" ]) ])
        , eq_body )
    ; Return (Hole (Array, "eq_result"))
    ] )

let gt_body =
  [ Assign
      ( PIndex (PHole (Array, "gt_result"), Hole (Number, "gt_i"))
      , Call
          ( Name ">"
          , [ Index (Name "gt_1", Hole (Number, "gt_i"))
            ; Index (Name "gt_2", Hole (Number, "gt_i"))
            ] ) )
  ]

let gt_defn =
  ( [ "gt_1"; "gt_2" ]
  , [ Assign
        ( PHole (Array, "gt_result")
        , Call (Name "zeros", [ Call (Name "len", [ Name "gt_1" ]) ]) )
    ; For
        ( PHole (Number, "gt_i")
        , Call (Name "range", [ Call (Name "len", [ Name "gt_1" ]) ])
        , gt_body )
    ; Return (Hole (Array, "gt_result"))
    ] )

let where_num_body =
  [ If
      ( Index (Name "cond", Hole (Number, "where_i"))
      , [ Assign
            ( PIndex (PHole (Array, "where_result"), Hole (Number, "where_i"))
            , Name "then" )
        ]
      , [ Assign
            ( PIndex (PHole (Array, "where_result"), Hole (Number, "where_i"))
            , Name "else" )
        ] )
  ]

let where_num_defn =
  ( [ "cond"; "then"; "else" ]
  , [ Assign
        ( PHole (Array, "where_result")
        , Call (Name "zeros", [ Call (Name "len", [ Hole (Array, "arr") ]) ]) )
    ; For
        ( PHole (Number, "where_i")
        , Call (Name "range", [ Call (Name "len", [ Hole (Array, "arr") ]) ])
        , where_num_body )
    ; Return (Hole (Array, "where_result"))
    ] )

let where_arr_body =
  [ If
      ( Index (Name "cond", Hole (Number, "where_i"))
      , [ Assign
            ( PIndex (PHole (Array, "where_result"), Hole (Number, "where_i"))
            , Index (Name "then", Hole (Number, "where_i")) )
        ]
      , [ Assign
            ( PIndex (PHole (Array, "where_result"), Hole (Number, "where_i"))
            , Index (Name "else", Hole (Number, "where_i")) )
        ] )
  ]

let where_arr_defn =
  ( [ "cond"; "then"; "else" ]
  , [ Assign
        ( PHole (Array, "where_result")
        , Call (Name "zeros", [ Call (Name "len", [ Hole (Array, "arr") ]) ]) )
    ; For
        ( PHole (Number, "where_i")
        , Call (Name "range", [ Call (Name "len", [ Hole (Array, "arr") ]) ])
        , where_arr_body )
    ; Return (Hole (Array, "where_result"))
    ] )

let roll_body =
  [ Assign
      ( PIndex (PHole (Array, "roll_result"), Hole (Number, "roll_i"))
      , Index
          (Name "x", Call (Name "-", [ Hole (Number, "roll_i"); Name "shift" ]))
      )
  ]

let roll_defn =
  ( [ "x"; "shift" ]
  , [ Assign
        ( PHole (Array, "roll_result")
        , Call (Name "zeros", [ Call (Name "len", [ Name "x" ]) ]) )
    ; For
        ( PHole (Number, "roll_i")
        , Call (Name "range", [ Call (Name "len", [ Name "x" ]) ])
        , roll_body )
    ; Return (Hole (Array, "roll_result"))
    ] )

let convolve_valid_defn =
  ( [ "x"; "h" ]
  , [ Assign
        ( PHole (Array, "conv_result")
        , Call
            ( Name "zeros"
            , [ Call
                  ( Name "+"
                  , [ Call
                        ( Name "-"
                        , [ Call (Name "len", [ Name "x" ])
                          ; Call (Name "len", [ Name "h" ])
                          ] )
                    ; Num 1
                    ] )
              ] ) )
    ; For
        ( PHole (Number, "conv_i")
        , Call
            ( Name "range"
            , [ Call
                  ( Name "+"
                  , [ Call
                        ( Name "-"
                        , [ Call (Name "len", [ Name "x" ])
                          ; Call (Name "len", [ Name "h" ])
                          ] )
                    ; Num 1
                    ] )
              ] )
        , [ Assign (PHole (Number, "conv_sum"), Num 0)
          ; For
              ( PHole (Number, "conv_j")
              , Call (Name "range", [ Call (Name "len", [ Name "h" ]) ])
              , [ Assign
                    ( PHole (Number, "conv_sum")
                    , Call
                        ( Name "+"
                        , [ Hole (Number, "conv_sum")
                          ; Call
                              ( Name "*"
                              , [ Index
                                    ( Name "x"
                                    , Call
                                        ( Name "+"
                                        , [ Hole (Number, "conv_i")
                                          ; Hole (Number, "conv_j")
                                          ] ) )
                                ; Index
                                    ( Name "h"
                                    , Call
                                        ( Name "-"
                                        , [ Call
                                              ( Name "-"
                                              , [ Call (Name "len", [ Name "h" ])
                                                ; Hole (Number, "conv_j")
                                                ] )
                                          ; Num 1
                                          ] ) )
                                ] )
                          ] ) )
                ] )
          ; Assign
              ( PIndex (PHole (Number, "conv_result"), Hole (Number, "conv_i"))
              , Hole (Number, "conv_sum") )
          ] )
    ; Return (Hole (Number, "conv_result"))
    ] )

let np_env : env =
  String.Map.of_alist_exn
    [ ("sum", sum_defn)
    ; ("mul", mul_defn)
    ; ("div", div_defn)
    ; ("add", add_defn)
    ; ("ones", ones_defn)
    ; ("eq", eq_defn)
    ; ("gt", gt_defn)
    ; ("where", where_arr_defn)
    ; ("roll", roll_defn)
    ; ("convolve_valid", convolve_valid_defn)
    ]
