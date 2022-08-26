open Core
open Lib
open Lang

let parse_file : string -> env * exp =
 fun filename ->
  In_channel.with_file ("../../../test/" ^ filename) ~f:(fun file ->
      Parse.program (In_channel.input_all file))

let actual_env1 =
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

let actual_main1 =
  EAbs
    ( "f"
    , EAbs
        ( "mx"
        , EApp
            ( EApp (EVar "withDefault", EVar "zero")
            , EApp (EApp (EVar "map", EVar "f"), EVar "mx") ) ) )

let parsed_env1, parsed_main1 = parse_file "test_programs/test1.lisp"

let%test "parse program 1 (env)" =
  Map.equal [%equal: exp] parsed_env1 actual_env1

let%test_unit "parse program 1 (main)" =
  [%test_eq: exp] parsed_main1 actual_main1
