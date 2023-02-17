open Core
open Cbr_fp
open Lang

let expected_datatype_env1 =
  String.Map.of_alist_exn
    [ ("Peano", [ ("Zero", TUnit); ("Succ", TDatatype "Peano") ])
    ; ("MaybePeano", [ ("Nothing", TUnit); ("Just", TDatatype "Peano") ])
    ]

let expected_typ_env1 =
  String.Map.of_alist_exn
    [ ("zero", TDatatype "Peano")
    ; ( "map"
      , TArr
          ( TArr (TDatatype "Peano", TDatatype "Peano")
          , TArr (TDatatype "MaybePeano", TDatatype "MaybePeano") ) )
    ; ( "withDefault"
      , TArr
          (TDatatype "Peano", TArr (TDatatype "MaybePeano", TDatatype "Peano"))
      )
    ; ( "main"
      , TArr
          ( TArr (TDatatype "Peano", TDatatype "Peano")
          , TArr (TDatatype "MaybePeano", TDatatype "Peano") ) )
    ]

let expected_env1 =
  String.Map.of_alist_exn
    [ ("zero", ECtor ("Zero", EUnit))
    ; ( "map"
      , EAbs
          ( "f"
          , TArr (TDatatype "Peano", TDatatype "Peano")
          , EAbs
              ( "mx"
              , TDatatype "MaybePeano"
              , EMatch
                  ( EVar "mx"
                  , [ ("Nothing", ("n", ECtor ("Nothing", EVar "n")))
                    ; ("Just", ("x", ECtor ("Just", EApp (EVar "f", EVar "x"))))
                    ] ) ) ) )
    ; ( "withDefault"
      , EAbs
          ( "default"
          , TDatatype "Peano"
          , EAbs
              ( "mx"
              , TDatatype "MaybePeano"
              , EMatch
                  ( EVar "mx"
                  , [ ("Nothing", ("n", EVar "default"))
                    ; ("Just", ("x", EVar "x"))
                    ] ) ) ) )
    ; ( "main"
      , EAbs
          ( "f"
          , TArr (TDatatype "Peano", TDatatype "Peano")
          , EAbs
              ( "mx"
              , TDatatype "MaybePeano"
              , EApp
                  ( EApp (EVar "withDefault", EVar "zero")
                  , EApp (EApp (EVar "map", EVar "f"), EVar "mx") ) ) ) )
    ]

let parsed_datatype_env1, parsed_typ_env1, parsed_env1 =
  Common.parse_file "programs/test1.lisp"

let%test_unit "parse program 1 (datatype_env)" =
  [%test_result: (id * (string * typ) list) list]
    (Map.to_alist parsed_datatype_env1)
    ~expect:(Map.to_alist expected_datatype_env1)

let%test_unit "parse program 1 (typ_env)" =
  [%test_result: (id * typ) list]
    (Map.to_alist parsed_typ_env1)
    ~expect:(Map.to_alist expected_typ_env1)

let%test_unit "parse program 1 (env)" =
  [%test_result: (id * exp) list]
    (Map.to_alist parsed_env1)
    ~expect:(Map.to_alist expected_env1)

let%test_unit "parse with ints 1" =
  [%test_result: exp]
    (Parse.exp "(succ -4)")
    ~expect:(EApp (EVar "succ", EInt (-4)))

let%test_unit "parse with ints 2" =
  [%test_result: exp]
    (Parse.exp "(add 1 -3)")
    ~expect:(EApp (EApp (EVar "add", EInt 1), EInt (-3)))

let%test_unit "list1 parses" = ignore (Common.parse_file "programs/list1.lisp")