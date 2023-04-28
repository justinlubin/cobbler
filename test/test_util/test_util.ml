open Core

let%test_unit "dedup_by 1" =
  [%test_result: int list]
    (Util.dedup_by ~f:(fun x y -> x mod 2 = y mod 2) [ 4; 6; 5; 2; 3; 1 ])
    ~expect:[ 4; 5 ]

let%test_unit "find_and_remove_first 1" =
  [%test_result: (int * int list) option]
    (Util.find_and_remove_first ~f:(fun n -> Int.equal n 2) [ 0; 1; 2; 3; 2 ])
    ~expect:(Some (2, [ 0; 1; 3; 2 ]))

let%test_unit "ungensym 1" =
  [%test_result: string * string]
    (Util.ungensym (Util.gensym "hello"), Util.ungensym (Util.gensym "hello"))
    ~expect:("hello", "hello")
