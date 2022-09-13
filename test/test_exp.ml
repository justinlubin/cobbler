open Core
open Lib
open Lang

let%test_unit "substitute 1" =
  [%test_result: exp]
    ~equal:Exp.alpha_equivalent
    (Exp.substitute ("x", EVar "y") (EAbs ("x", EVar "x")))
    ~expect:(EAbs ("x", EVar "x"))

let%test_unit "substitute 2" =
  [%test_result: exp]
    ~equal:Exp.alpha_equivalent
    (Exp.substitute ("z", EVar "x") (EAbs ("x", EVar "z")))
    ~expect:(EAbs ("__var0", EVar "x"))

let%test_unit "substitute 3" =
  [%test_result: exp]
    ~equal:Exp.alpha_equivalent
    (Exp.substitute ("x", EVar "y") (EAbs ("x", EVar "x")))
    ~expect:(EAbs ("x", EVar "x"))

let%test_unit "alpha equivalent 1" =
  [%test_result: exp]
    ~equal:Exp.alpha_equivalent
    (EAbs ("x", EVar "x"))
    ~expect:(EAbs ("y", EVar "y"))
