open Core
open Lib
open Lang

let%test_unit "substitute 1" =
  [%test_result: exp]
    ~equal:Exp.alpha_equivalent
    (Exp.substitute ("x", EVar "y") (EAbs ("x", TPlaceholder "Int", EVar "x")))
    ~expect:(EAbs ("x", TPlaceholder "Int", EVar "x"))

let%test_unit "substitute 2" =
  [%test_result: exp]
    ~equal:Exp.alpha_equivalent
    (Exp.substitute ("z", EVar "x") (EAbs ("x", TPlaceholder "Int", EVar "z")))
    ~expect:(EAbs ("__var0", TPlaceholder "Int", EVar "x"))

let%test_unit "substitute 3" =
  [%test_result: exp]
    ~equal:Exp.alpha_equivalent
    (Exp.substitute ("x", EVar "y") (EAbs ("x", TPlaceholder "Int", EVar "x")))
    ~expect:(EAbs ("x", TPlaceholder "Int", EVar "x"))

let%test_unit "alpha equivalent 1" =
  [%test_result: exp]
    ~equal:Exp.alpha_equivalent
    (EAbs ("x", TPlaceholder "Int", EVar "x"))
    ~expect:(EAbs ("y", TPlaceholder "Int", EVar "y"))
