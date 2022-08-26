open Core
open Lib
open Lang

let testenv1 : env =
  Map.of_alist_exn
    (module String)
    [ ("foo", EAbs ("x", EVar "x"))
    ; ("bar", EAbs ("x", ECtor ("Just", EVar "x")))
    ; ("baz", EAbs ("u", EVar "bar"))
    ; ("zero", EAbs ("z", EVar "z"))
    ]

let%test_unit "closed_substitute 1" =
  [%test_eq: exp]
    (Lang_util.closed_substitute testenv1 (EAbs ("x", EVar "x")))
    (EAbs ("x", EVar "x"))

let%test_unit "closed_substitute 2" =
  [%test_eq: exp]
    (Lang_util.closed_substitute testenv1 (EAbs ("x", EVar "foo")))
    (EAbs ("x", EAbs ("x", EVar "x")))

let%test_unit "closed_substitute 3" =
  [%test_eq: exp]
    (Lang_util.closed_substitute testenv1 (EAbs ("foo", EVar "foo")))
    (EAbs ("foo", EVar "foo"))

let%test_unit "closed_substitute 4" =
  [%test_eq: exp]
    (Lang_util.closed_substitute
       testenv1
       (EMatch
          ( ECtor ("Just", EVar "foo")
          , [ ("Nothing", ("x", EVar "zero")); ("Just", ("foo", EVar "foo")) ]
          )))
    (EMatch
       ( ECtor ("Just", EAbs ("x", EVar "x"))
       , [ ("Nothing", ("x", EAbs ("z", EVar "z")))
         ; ("Just", ("foo", EVar "foo"))
         ] ))

let%test_unit "closed_substitute 5" =
  [%test_eq: exp]
    (Lang_util.closed_substitute
       testenv1
       (EAbs ("hello", EApp (EVar "bar", EVar "hello"))))
    (EAbs ("hello", EApp (EAbs ("x", ECtor ("Just", EVar "x")), EVar "hello")))

let%test_unit "closed_substitute 6" =
  [%test_eq: exp]
    (Lang_util.closed_substitute
       testenv1
       (EAbs ("hello", EApp (EVar "foo", EVar "hello"))))
    (EAbs ("hello", EApp (EAbs ("x", EVar "x"), EVar "hello")))

let%test_unit "closed_substitute 7" =
  [%test_eq: exp]
    (Lang_util.closed_substitute testenv1 (EVar "baz"))
    (EAbs ("u", EAbs ("x", ECtor ("Just", EVar "x"))))
