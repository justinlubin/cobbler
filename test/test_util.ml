open Core
open Lib

let%test_unit "dedup_by 1" =
  [%test_eq: int list]
    (Util.dedup_by ~f:(fun x y -> x mod 2 = y mod 2) [ 4; 6; 5; 2; 3; 1 ])
    [ 4; 5 ]
