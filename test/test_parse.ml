open Core
open Lib
open Lang

let expected_env1 =
  Map.of_alist_exn
    (module String)
    [ ("zero", ECtor ("Zero", EAbs ("z", EVar "z")))
    ; ( "map"
      , EAbs
          ( "f"
          , EAbs
              ( "mx"
              , EMatch
                  ( EVar "mx"
                  , [ ("Nothing", ("n", ECtor ("Nothing", EVar "n")))
                    ; ("Just", ("x", ECtor ("Just", EApp (EVar "f", EVar "x"))))
                    ] ) ) ) )
    ; ( "withDefault"
      , EAbs
          ( "default"
          , EAbs
              ( "mx"
              , EMatch
                  ( EVar "mx"
                  , [ ("Nothing", ("n", EVar "default"))
                    ; ("Just", ("x", EVar "x"))
                    ] ) ) ) )
    ]

let expected_main1 =
  EAbs
    ( "f"
    , EAbs
        ( "mx"
        , EApp
            ( EApp (EVar "withDefault", EVar "zero")
            , EApp (EApp (EVar "map", EVar "f"), EVar "mx") ) ) )

let parsed_env1, parsed_main1 = Common.parse_file "programs/test1.lisp"

let%test "parse program 1 (env)" =
  Map.equal [%equal: exp] parsed_env1 expected_env1

let%test_unit "parse program 1 (main)" =
  [%test_result: exp] parsed_main1 ~expect:expected_main1

let%test_unit "parse with ints 1" =
  [%test_result: exp]
    (Parse.exp "(succ -4)")
    ~expect:(EApp (EVar "succ", EInt (-4)))

let%test_unit "parse with ints 2" =
  [%test_result: exp]
    (Parse.exp "(add 1 -3)")
    ~expect:(EApp (EApp (EVar "add", EInt 1), EInt (-3)))
