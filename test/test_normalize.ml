open Core
open Lib
open Lang

let%test_unit "partially evaluate cases 1" =
  [%test_eq: exp]
    (Normalize.partially_evaluate_cases
       (EMatch
          ( ECtor ("Just", EVar "x")
          , [ ("Nothing", ("y", EVar "zero"))
            ; ("Just", ("z", ECtor ("Ok", EVar "z")))
            ] )))
    (ECtor ("Ok", EVar "x"))

let%test_unit "pull out cases 1" =
  [%test_eq: exp]
    (Normalize.pull_out_cases
       (EMatch
          ( EMatch
              ( EVar "mx"
              , [ ("Nothing", ("n1", ECtor ("Nothing", EVar "n1")))
                ; ("Just", ("x", ECtor ("Just", EApp (EVar "f", EVar "x"))))
                ] )
          , [ ("Nothing", ("n2", EVar "zero")); ("Just", ("y", EVar "y")) ] )))
    (EMatch
       ( EVar "mx"
       , [ ( "Nothing"
           , ( "n1"
             , EMatch
                 ( ECtor ("Nothing", EVar "n1")
                 , [ ("Nothing", ("n2", EVar "zero"))
                   ; ("Just", ("y", EVar "y"))
                   ] ) ) )
         ; ( "Just"
           , ( "x"
             , EMatch
                 ( ECtor ("Just", EApp (EVar "f", EVar "x"))
                 , [ ("Nothing", ("n2", EVar "zero"))
                   ; ("Just", ("y", EVar "y"))
                   ] ) ) )
         ] ))

let test_library : env =
  Map.of_alist_exn
    (module String)
    [ ( "map"
      , EAbs
          ( "map_f"
          , EAbs
              ( "map_mx"
              , EMatch
                  ( EVar "map_mx"
                  , [ ("Nothing", ("map_n", ECtor ("Nothing", EVar "map_n")))
                    ; ( "Just"
                      , ( "map_x"
                        , ECtor ("Just", EApp (EVar "map_f", EVar "map_x")) ) )
                    ] ) ) ) )
    ; ( "withDefault"
      , EAbs
          ( "default"
          , EAbs
              ( "wd_mx"
              , EMatch
                  ( EVar "wd_mx"
                  , [ ("Nothing", ("wd_n", EVar "default"))
                    ; ("Just", ("wd_x", EVar "wd_x"))
                    ] ) ) ) )
    ]

let%test_unit "pull out cases 1" =
  [%test_eq: exp]
    (Normalize.full
       test_library
       (EAbs
          ( "f"
          , EAbs
              ( "mx"
              , EApp
                  ( EApp (EVar "withDefault", EAbs ("zero", EVar "zero"))
                  , EApp (EApp (EVar "map", EVar "f"), EVar "mx") ) ) )))
    (EAbs
       ( "f"
       , EAbs
           ( "mx"
           , EMatch
               ( EVar "mx"
               , [ ("Nothing", ("map_n", EAbs ("zero", EVar "zero")))
                 ; ("Just", ("map_x", EApp (EVar "f", EVar "map_x")))
                 ] ) ) ))
