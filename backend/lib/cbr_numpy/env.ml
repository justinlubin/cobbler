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

let prod_body =
  [ Assign
      ( PHole (Number, "prod_count")
      , Call
          ( Name "*"
          , [ Hole (Number, "prod_count")
            ; Index (Name "prod_1", Hole (Number, "prod_i"))
            ] ) )
  ]

let prod_defn =
  ( [ "prod_1" ]
  , [ Assign (PHole (Number, "prod_count"), Num 1)
    ; For
        ( PHole (Number, "prod_i")
        , Call (Name "range", [ Call (Name "len", [ Name "prod_1" ]) ])
        , prod_body )
    ; Return (Hole (Number, "prod_count"))
    ] )

let sub_body =
  [ Assign
      ( PHole (Number, "sub_count")
      , Call
          ( Name "-"
          , [ Hole (Number, "sub_count")
            ; Index (Name "sub_1", Hole (Number, "sub_i"))
            ] ) )
  ]

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
        , Call (Name "np.zeros", [ Call (Name "len", [ Name "mul_1" ]) ]) )
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
        , Call (Name "np.zeros", [ Call (Name "len", [ Name "add_1" ]) ]) )
    ; For
        ( PHole (Number, "add_i")
        , Call (Name "range", [ Call (Name "len", [ Name "add_1" ]) ])
        , add_body )
    ; Return (Hole (Array, "add_result"))
    ] )

let subtract_body =
  [ Assign
      ( PIndex (PHole (Array, "subtract_result"), Hole (Number, "subtract_i"))
      , Call
          ( Name "-"
          , [ Index (Name "subtract_1", Hole (Number, "subtract_i"))
            ; Index (Name "subtract_2", Hole (Number, "subtract_i"))
            ] ) )
  ]

let subtract_defn =
  ( [ "subtract_1"; "subtract_2" ]
  , [ Assign
        ( PHole (Array, "subtract_result")
        , Call (Name "np.zeros", [ Call (Name "len", [ Name "subtract_1" ]) ])
        )
    ; For
        ( PHole (Number, "subtract_i")
        , Call (Name "range", [ Call (Name "len", [ Name "subtract_1" ]) ])
        , subtract_body )
    ; Return (Hole (Array, "subtract_result"))
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
        , Call (Name "np.zeros", [ Call (Name "len", [ Name "div_1" ]) ]) )
    ; For
        ( PHole (Number, "div_i")
        , Call (Name "range", [ Call (Name "len", [ Name "div_1" ]) ])
        , div_body )
    ; Return (Hole (Array, "div_result"))
    ] )

let power_body =
  [ Assign
      ( PIndex (PHole (Array, "pow_result"), Hole (Number, "pow_i"))
      , Call
          ( Name "**"
          , [ Index (Name "pow_1", Hole (Number, "pow_i"))
            ; Index (Name "pow_2", Hole (Number, "pow_i"))
            ] ) )
  ]

let power_defn =
  ( [ "pow_1"; "pow_2" ]
  , [ Assign
        ( PHole (Array, "pow_result")
        , Call (Name "np.zeros", [ Call (Name "len", [ Name "pow_1" ]) ]) )
    ; For
        ( PHole (Number, "pow_i")
        , Call (Name "range", [ Call (Name "len", [ Name "pow_1" ]) ])
        , power_body )
    ; Return (Hole (Array, "pow_result"))
    ] )

let full_defn =
  ( [ "size"; "value" ]
  , [ Assign
        (PHole (Array, "full_result"), Call (Name "np.zeros", [ Name "size" ]))
    ; For
        ( PHole (Number, "full_i")
        , Call (Name "range", [ Name "size" ])
        , [ Assign
              ( PIndex (PHole (Array, "full_result"), Hole (Number, "full_i"))
              , Name "value" )
          ] )
    ; Return (Hole (Array, "full_result"))
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
        , Call (Name "np.zeros", [ Call (Name "len", [ Name "eq_1" ]) ]) )
    ; For
        ( PHole (Number, "eq_i")
        , Call (Name "range", [ Call (Name "len", [ Name "eq_1" ]) ])
        , eq_body )
    ; Return (Hole (Array, "eq_result"))
    ] )

let neq_body =
  [ Assign
      ( PIndex (PHole (Array, "neq_result"), Hole (Number, "neq_i"))
      , Call
          ( Name "!="
          , [ Index (Name "neq_1", Hole (Number, "neq_i"))
            ; Index (Name "neq_2", Hole (Number, "neq_i"))
            ] ) )
  ]

let neq_defn =
  ( [ "neq_1"; "neq_2" ]
  , [ Assign
        ( PHole (Array, "neq_result")
        , Call (Name "np.zeros", [ Call (Name "len", [ Name "neq_1" ]) ]) )
    ; For
        ( PHole (Number, "neq_i")
        , Call (Name "range", [ Call (Name "len", [ Name "neq_1" ]) ])
        , neq_body )
    ; Return (Hole (Array, "neq_result"))
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
        , Call (Name "np.zeros", [ Call (Name "len", [ Name "gt_1" ]) ]) )
    ; For
        ( PHole (Number, "gt_i")
        , Call (Name "range", [ Call (Name "len", [ Name "gt_1" ]) ])
        , gt_body )
    ; Return (Hole (Array, "gt_result"))
    ] )

let gte_body =
  [ Assign
      ( PIndex (PHole (Array, "gte_result"), Hole (Number, "gte_i"))
      , Call
          ( Name ">="
          , [ Index (Name "gte_1", Hole (Number, "gte_i"))
            ; Index (Name "gte_2", Hole (Number, "gte_i"))
            ] ) )
  ]

let gte_defn =
  ( [ "gte_1"; "gte_2" ]
  , [ Assign
        ( PHole (Array, "gte_result")
        , Call (Name "np.zeros", [ Call (Name "len", [ Name "gte_1" ]) ]) )
    ; For
        ( PHole (Number, "gte_i")
        , Call (Name "range", [ Call (Name "len", [ Name "gte_1" ]) ])
        , gte_body )
    ; Return (Hole (Array, "gte_result"))
    ] )

let lt_body =
  [ Assign
      ( PIndex (PHole (Array, "lt_result"), Hole (Number, "lt_i"))
      , Call
          ( Name "<"
          , [ Index (Name "lt_1", Hole (Number, "lt_i"))
            ; Index (Name "lt_2", Hole (Number, "lt_i"))
            ] ) )
  ]

let lt_defn =
  ( [ "lt_1"; "lt_2" ]
  , [ Assign
        ( PHole (Array, "lt_result")
        , Call (Name "np.zeros", [ Call (Name "len", [ Name "lt_1" ]) ]) )
    ; For
        ( PHole (Number, "lt_i")
        , Call (Name "range", [ Call (Name "len", [ Name "lt_1" ]) ])
        , lt_body )
    ; Return (Hole (Array, "lt_result"))
    ] )

let lte_body =
  [ Assign
      ( PIndex (PHole (Array, "lte_result"), Hole (Number, "lte_i"))
      , Call
          ( Name "<="
          , [ Index (Name "lte_1", Hole (Number, "lte_i"))
            ; Index (Name "lte_2", Hole (Number, "lte_i"))
            ] ) )
  ]

let lte_defn =
  ( [ "lte_1"; "lte_2" ]
  , [ Assign
        ( PHole (Array, "lte_result")
        , Call (Name "np.zeros", [ Call (Name "len", [ Name "lte_1" ]) ]) )
    ; For
        ( PHole (Number, "lte_i")
        , Call (Name "range", [ Call (Name "len", [ Name "lte_1" ]) ])
        , lte_body )
    ; Return (Hole (Array, "lte_result"))
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
        , Call (Name "np.zeros", [ Call (Name "len", [ Hole (Array, "arr") ]) ])
        )
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
        , Call (Name "np.zeros", [ Call (Name "len", [ Hole (Array, "arr") ]) ])
        )
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
        , Call (Name "np.zeros", [ Call (Name "len", [ Name "x" ]) ]) )
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
            ( Name "np.zeros"
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

let randint_size_defn =
  ( [ "low"; "high"; "size" ]
  , [ Assign (PHole (Array, "x"), Call (Name "np.zeros", [ Name "size" ]))
    ; For
        ( PHole (Number, "i")
        , Call (Name "range", [ Name "size" ])
        , [ Assign
              ( PIndex (PHole (Array, "x"), Hole (Number, "i"))
              , Call (Name "np.random.randint", [ Name "low"; Name "high" ]) )
          ] )
    ; Return (Hole (Array, "x"))
    ] )

let tolist_defn =
  ( [ "arg"; "amount" ]
  , [ Assign (PHole (List, "xs"), Name "__emptyList")
    ; For
        ( PHole (Number, "i")
        , Call (Name "range", [ Name "amount" ])
        , [ Assign
              ( PHole (List, "xs")
              , Call
                  ( Name "np.append"
                  , [ Hole (List, "xs")
                    ; Index (Name "arg", Hole (Number, "i"))
                    ] ) )
          ] )
    ; Return (Hole (Array, "xs"))
    ] )

let copy_defn =
  ( [ "arg"; "amount" ]
  , [ Assign (PHole (Array, "y"), Call (Name "np.zeros", [ Name "amount" ]))
    ; For
        ( PHole (Number, "i")
        , Call (Name "range", [ Name "amount" ])
        , [ Assign
              ( PIndex (PHole (Array, "y"), Hole (Number, "i"))
              , Index (Name "arg", Hole (Number, "i")) )
          ] )
    ; Return (Hole (Array, "y"))
    ] )

(* let arange_defn =
  ( [ "hi" ]
  , [ Assign (PHole (Array, "x"), Call (Name "np.zeros", [ Name "hi" ]))
    ; For
        ( PHole (Number, "i")
        , Call (Name "range", [ Name "hi" ])
        , [ Assign
              ( PIndex (PHole (Array, "x"), Hole (Number, "i"))
              , Hole (Number, "i") )
          ] )
    ; Return (Hole (Array, "x"))
    ] ) *)

let sum_string_body =
  [ Assign
      ( PHole (String, "sum_string_count")
      , Call
          ( Name "+"
          , [ Hole (String, "sum_string_count")
            ; Index (Name "sum_string_1", Hole (String, "sum_string_i"))
            ] ) )
  ]

let sum_string_defn =
  ( [ "sum_string_1" ]
  , [ Assign (PHole (String, "sum_string_count"), Str "")
    ; For
        ( PHole (String, "sum_string_i")
        , Call (Name "range", [ Call (Name "len", [ Name "sum_string_1" ]) ])
        , sum_string_body )
    ; Return (Hole (String, "sum_string_count"))
    ] )

let filter_defn =
  ( [ "array"; "pred" ]
  , [ Assign (PHole (List, "xs"), Name "__emptyList")
    ; For
        ( PHole (Number, "i")
        , Call (Name "range", [ Call (Name "len", [ Name "array" ]) ])
        , [ If
              ( Name "pred"
              , [ Assign
                    ( PHole (List, "xs")
                    , Call
                        ( Name "np.append"
                        , [ Hole (List, "xs")
                          ; Index (Name "array", Hole (Number, "i"))
                          ] ) )
                ]
              , [] )
          ] )
    ; Return (Hole (Array, "xs"))
    ] )

let square_defn =
  ( ["array"]
  , [ Assign 
      ( PHole (Array, "square_result")
      , Call (Name "np.zeros", [ Call (Name "len", [ Name "array" ]) ])
      )
    ; For
      ( PHole (Number, "square_i")
      , Call (Name "range", [ Call (Name "len", [ Name "array" ]) ])
      , [ Assign 
          ( PIndex (PHole (Array, "square_result"), Hole (Number, "square_i"))
          , Call
              ( Name "**"
              , [ Index (Name "array", Hole (Number, "square_i"))
                ; Num 2
                ] ) )
        ] )
    ; Return (Hole (Array, "square_result"))
  ])

let np_env : env =
  String.Map.of_alist_exn
    [ ("np.sum", sum_defn)
    ; ("np.prod", prod_defn)
    ; ("np.multiply", mul_defn)
    ; ("np.divide", div_defn)
    ; ("np.add", add_defn)
    ; ("np.subtract", subtract_defn)
    ; ("np.power", power_defn)
    ; ("np.full", full_defn)
    ; ("np.equal", eq_defn)
    ; ("np.not_equal", neq_defn)
    ; ("np.greater", gt_defn)
    ; ("np.greater_equal", gt_defn)
    ; ("np.less", gt_defn)
    ; ("np.less_equal", gt_defn)
    ; ("np.where", where_arr_defn)
    ; ("np.roll", roll_defn)
    ; ("np.convolve_valid", convolve_valid_defn)
    ; ("np.random.randint_size", randint_size_defn)
    ; ("np.tolist", tolist_defn) (* ; ("np.arange", arange_defn) *)
    ; ("np.copy", copy_defn)
    ; ("np.filter", filter_defn)
    ; ("np.square", square_defn)
    ]
