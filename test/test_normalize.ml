open Core
open Lib
open Lang

let test_env =
  Map.of_alist_exn
    (module String)
    [ ("zero", EAbs ("z", EVar "z")); ("foo", EAbs ("y", EVar "y")) ]

let%test_unit "inline 1" =
  [%test_eq: exp]
    (Normalize.inline test_env (EAbs ("x", EVar "x")))
    (EAbs ("x", EVar "x"))

let%test_unit "inline 2" =
  [%test_eq: exp]
    (Normalize.inline test_env (EAbs ("x", EVar "foo")))
    (EAbs ("x", EAbs ("y", EVar "y")))

let%test_unit "inline 3" =
  [%test_eq: exp]
    (Normalize.inline test_env (EAbs ("foo", EVar "foo")))
    (EAbs ("foo", EVar "foo"))

let%test_unit "inline 4" =
  [%test_eq: exp]
    (Normalize.inline
       test_env
       (EMatch
          ( ECtor ("Just", EVar "foo")
          , [ ("Nothing", ("x", EVar "zero")); ("Just", ("foo", EVar "foo")) ]
          )))
    (EMatch
       ( ECtor ("Just", EAbs ("y", EVar "y"))
       , [ ("Nothing", ("x", EAbs ("z", EVar "z")))
         ; ("Just", ("foo", EVar "foo"))
         ] ))

let%test_unit "partially evaluate cases 1" =
  [%test_eq: exp]
    (Normalize.partially_evaluate_cases
       (EMatch
          ( ECtor ("Just", EVar "x")
          , [ ("Nothing", ("y", EVar "zero"))
            ; ("Just", ("z", ECtor ("Ok", EVar "z")))
            ] )))
    (ECtor ("Ok", EVar "x"))
