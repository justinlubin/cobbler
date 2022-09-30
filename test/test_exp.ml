open Core
open Lib
open Lang

let%test_unit "substitute 1" =
  [%test_result: exp]
    ~equal:Exp.alpha_equivalent
    (Exp.substitute ("x", EVar "y") (EAbs ("x", TInt, EVar "x")))
    ~expect:(EAbs ("x", TInt, EVar "x"))

let%test_unit "substitute 2" =
  [%test_result: exp]
    ~equal:Exp.alpha_equivalent
    (Exp.substitute ("z", EVar "x") (EAbs ("x", TInt, EVar "z")))
    ~expect:(EAbs ("__var0", TInt, EVar "x"))

let%test_unit "substitute 3" =
  [%test_result: exp]
    ~equal:Exp.alpha_equivalent
    (Exp.substitute ("x", EVar "y") (EAbs ("x", TInt, EVar "x")))
    ~expect:(EAbs ("x", TInt, EVar "x"))

let%test_unit "alpha equivalent 1" =
  [%test_result: exp]
    ~equal:Exp.alpha_equivalent
    (EAbs ("x", TInt, EVar "x"))
    ~expect:(EAbs ("y", TInt, EVar "y"))

let%test_unit "normalizes cases 1" =
  [%test_eq: exp]
    (Exp.normalize
       (EMatch
          ( ECtor ("Just", EVar "x")
          , [ ("Nothing", ("y", EVar "zero"))
            ; ("Just", ("z", ECtor ("Ok", EVar "z")))
            ] )))
    (ECtor ("Ok", EVar "x"))