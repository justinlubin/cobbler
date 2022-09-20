open Core
open Lib
open Lang

let test_library1 : env =
  Map.of_alist_exn
    (module String)
    [ ( "map"
      , EAbs
          ( "map_f"
          , TArr (TDatatype "Peano", TDatatype "Peano")
          , EAbs
              ( "map_mx"
              , TDatatype "MaybePeano"
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
          , TDatatype "Peano"
          , EAbs
              ( "wd_mx"
              , TDatatype "MaybePeano"
              , EMatch
                  ( EVar "wd_mx"
                  , [ ("Nothing", ("wd_n", EVar "default"))
                    ; ("Just", ("wd_x", EVar "wd_x"))
                    ] ) ) ) )
    ]

let%test_unit "norm 1" =
  [%test_eq: exp]
    (Synthesis.norm
       test_library1
       (EAbs
          ( "f"
          , TArr (TDatatype "Peano", TDatatype "Peano")
          , EAbs
              ( "mx"
              , TDatatype "MaybePeano"
              , EApp
                  ( EApp (EVar "withDefault", EAbs ("zero", TUnit, EVar "zero"))
                  , EApp (EApp (EVar "map", EVar "f"), EVar "mx") ) ) )))
    (EAbs
       ( "f"
       , TArr (TDatatype "Peano", TDatatype "Peano")
       , EAbs
           ( "mx"
           , TDatatype "MaybePeano"
           , EMatch
               ( EVar "mx"
               , [ ("Nothing", ("map_n", EAbs ("zero", TUnit, EVar "zero")))
                 ; ("Just", ("map_x", EApp (EVar "f", EVar "map_x")))
                 ] ) ) ))

let%test_unit "classic synth 1" =
  let problem =
    Synthesis.problem_of_definitions (Common.parse_file "programs/classic.lisp")
  in
  let expected_solution =
    EApp
      ( EApp (EVar "withDefault", EVar "zero")
      , EApp (EApp (EVar "map", EVar "f"), EVar "mx") )
  in
  let actual_solution = Synthesis.solve problem |> Option.value_exn in
  [%test_result: exp] actual_solution ~expect:expected_solution
