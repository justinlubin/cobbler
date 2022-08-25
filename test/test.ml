open Core
open Lib

let%test_unit "rev" = [%test_eq: int list] (List.rev [ 3; 2; 1 ]) [ 1; 2; 3 ]

let%test_unit "string concat" =
  [%test_eq: string] ("hello " ^ "world") "hello world"

let%test_unit "property-based test" =
  Quickcheck.test
    ~sexp_of:[%sexp_of: int]
    (Int.gen_incl (Int.min_value + 1) Int.max_value)
    ~f:(fun x ->
      [%test_eq: Sign.t] (Int.sign (Int.neg x)) (Sign.flip (Int.sign x)))
