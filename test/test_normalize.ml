open Core
open Lib
open Lang

let test_lib = Map.of_alist_exn (module String) [ "foo", EAbs ("y", EVar "y") ]

let%test_unit "inline 1" =
  [%test_eq: exp]
    (Normalize.inline test_lib (EAbs ("x", EVar "x")))
    (Normalize.inline test_lib (EAbs ("x", EVar "x")))

let%test_unit "inline 2" =
  [%test_eq: exp]
    (Normalize.inline test_lib (EAbs ("x", EVar "foo")))
    (Normalize.inline test_lib (EAbs ("x", EAbs ("y", EVar "y"))))

let%test_unit "inline 3" =
  [%test_eq: exp]
    (Normalize.inline test_lib (EAbs ("foo", EVar "foo")))
    (Normalize.inline test_lib (EAbs ("foo", EVar "foo")))
