open Core
open Lib
open Lang

let%test "substitute 1" =
  Lang_util.alpha_equivalent
    (Lang_util.substitute ("x", EVar "y") (EAbs ("x", EVar "x")))
    (EAbs ("x", EVar "x"))

let%test "substitute 2" =
  Lang_util.alpha_equivalent
    (Lang_util.substitute ("z", EVar "x") (EAbs ("x", EVar "z")))
    (EAbs ("__var0", EVar "x"))

let%test "substitute 3" =
  Lang_util.alpha_equivalent
    (Lang_util.substitute ("x", EVar "y") (EAbs ("x", EVar "x")))
    (EAbs ("x", EVar "x"))

let%test "alpha equivalent 1" =
  Lang_util.alpha_equivalent (EAbs ("x", EVar "x")) (EAbs ("y", EVar "y"))
