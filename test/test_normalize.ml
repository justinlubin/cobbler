open Core
open Lib
open Lang

let testlib1 : library =
  Map.of_alist_exn
    (module String)
    [ ("zero", ([], EAbs ([ "z" ], EVar "z")))
    ; ("foo", ([], EAbs ([ "y" ], EVar "y")))
    ; ("bar", ([ "x" ], ECtor ("Just", EVar "x")))
    ]

let%test_unit "inline 1" =
  [%test_eq: exp]
    (Normalize.inline testlib1 (EAbs ([ "x" ], EVar "x")))
    (EAbs ([ "x" ], EVar "x"))

let%test_unit "inline 2" =
  [%test_eq: exp]
    (Normalize.inline testlib1 (EAbs ([ "x" ], EVar "foo")))
    (EAbs ([ "x" ], EAbs ([ "y" ], EVar "y")))

let%test_unit "inline 3" =
  [%test_eq: exp]
    (Normalize.inline testlib1 (EAbs ([ "foo" ], EVar "foo")))
    (EAbs ([ "foo" ], EVar "foo"))

let%test_unit "inline 4" =
  [%test_eq: exp]
    (Normalize.inline
       testlib1
       (EMatch
          ( ECtor ("Just", EVar "foo")
          , [ ("Nothing", ("x", EVar "zero")); ("Just", ("foo", EVar "foo")) ]
          )))
    (EMatch
       ( ECtor ("Just", EAbs ([ "y" ], EVar "y"))
       , [ ("Nothing", ("x", EAbs ([ "z" ], EVar "z")))
         ; ("Just", ("foo", EVar "foo"))
         ] ))

let%test_unit "inline 5" =
  [%test_eq: exp]
    (Normalize.inline
       testlib1
       (EAbs ([ "hello" ], EApp (EVar "bar", [ EVar "hello" ]))))
    (EAbs ([ "hello" ], ECtor ("Just", EVar "hello")))

let%test_unit "inline 6" =
  [%test_eq: exp]
    (Normalize.inline
       testlib1
       (EAbs ([ "hello" ], EApp (EVar "foo", [ EVar "hello" ]))))
    (EAbs ([ "hello" ], EApp (EAbs ([ "y" ], EVar "y"), [ EVar "hello" ])))

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
                ; ("Just", ("x", ECtor ("Just", EApp (EVar "f", [ EVar "x" ]))))
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
                 ( ECtor ("Just", EApp (EVar "f", [ EVar "x" ]))
                 , [ ("Nothing", ("n2", EVar "zero"))
                   ; ("Just", ("y", EVar "y"))
                   ] ) ) )
         ] ))

let testlib2 : library =
  Map.of_alist_exn
    (module String)
    [ ( "map"
      , ( [ "map_f"; "map_mx" ]
        , EMatch
            ( EVar "map_mx"
            , [ ("Nothing", ("map_n", ECtor ("Nothing", EVar "map_n")))
              ; ( "Just"
                , ( "map_x"
                  , ECtor ("Just", EApp (EVar "map_f", [ EVar "map_x" ])) ) )
              ] ) ) )
    ; ( "withDefault"
      , ( [ "default"; "wd_mx" ]
        , EMatch
            ( EVar "wd_mx"
            , [ ("Nothing", ("wd_n", EVar "default"))
              ; ("Just", ("wd_x", EVar "wd_x"))
              ] ) ) )
    ]

let%test_unit "pull out cases 1" =
  [%test_eq: exp]
    (Normalize.full
       testlib2
       (EAbs
          ( [ "f"; "mx" ]
          , EApp
              ( EVar "withDefault"
              , [ EAbs ([ "zero" ], EVar "zero")
                ; EApp (EVar "map", [ EVar "f"; EVar "mx" ])
                ] ) )))
    (EAbs
       ( [ "f"; "mx" ]
       , EMatch
           ( EVar "mx"
           , [ ("Nothing", ("map_n", EAbs ([ "zero" ], EVar "zero")))
             ; ("Just", ("map_x", EApp (EVar "f", [ EVar "map_x" ])))
             ] ) ))
