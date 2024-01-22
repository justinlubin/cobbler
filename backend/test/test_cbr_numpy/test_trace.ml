open Cbr_numpy
open Lang
open Partial_eval
open Parse
open Core
open Env
open Make_trace

let p =
  ( np_env
  , [ Return
        (Call (Name "len", [ Call (Name "np.multiply", [ Name "x"; Name "y" ]) ]))
    ] )

let trace =
  [ ( np_env
    , [ Return
          (Call (Name "len", [ Call (Name "np.multiply", [ Name "x"; Name "y" ]) ]))
      ] ),
    [ ( np_env
      , [ Return
            (Call (Name "len", [ Call (Name "np.multiply", [ Name "x"; Name "y" ]) ]))
        ] )
    ; ( np_env
      , [ Return
            (Call (Name "len", [ Name "x"]))
        ] )
    ]
  ; ( np_env
    , [ Return (Name "len")]),
    [ ( np_env 
      , [ Return (Name "len")])
    ]
  ; ( np_env
    , [ Return (Call (Name "np.multiply", [ Name "x"; Name "y" ]))]),
    [ ( np_env
      , [ Return (Call (Name "np.multiply", [ Name "x"; Name "y" ]))])
    ; ( np_env
      , [ Assign
            ( PHole (Array, "mul_result")
            , Call (Name "np.zeros", [ Call (Name "len", [ Name "x" ]) ]) )
        ; For
            ( PHole (Number, "mul_i")
            , Call (Name "range", [ Call (Name "len", [ Name "x" ]) ])
            , [ Assign
              ( PIndex (PHole (Array, "mul_result"), Hole (Number, "mul_i"))
              , Call
                  ( Name "*"
                  , [ Index (Name "x", Hole (Number, "mul_i"))
                    ; Index (Name "y", Hole (Number, "mul_i"))
                    ] ) )
              ] )
        ; Return (Hole (Array, "mul_result"))
        ] )
    ]
  ; ( np_env
    , [ Return (Name "np.multiply")]),
    [ ( np_env 
      , [ Return (Name "np.multiply")])
    ]
  ; ( np_env
    , [ Return (Name "x")]),
    [ ( np_env 
      , [ Return (Name "x")])
    ]
  ; ( np_env
    , [ Return (Name "y")]),
    [ ( np_env 
      , [ Return (Name "y")])
    ]
  ]

let%test_unit "make trace" =
  [%test_result: trace] (make_trace p) ~expect:trace
