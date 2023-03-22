open Core
open Cbr_numpy
open Parse
open Unification
open Lang

let reference1 : program =
  ( String.Map.of_alist_exn []
  , [ Assign (Name "x", Call (Name "+", [ Num 1; Num 2 ])) ] )

let reference2 : program =
  ( String.Map.of_alist_exn []
  , [ Assign
        (Name "x", Call (Name "+", [ Call (Name "*", [ Num 2; Num 3 ]); Num 2 ]))
    ] )

let candidate1 : program =
  ( String.Map.of_alist_exn []
  , [ Assign (Name "x", Call (Name "+", [ Hole (Number, "1"); Num 2 ])) ] )

let candidate2 : program =
  ( String.Map.of_alist_exn []
  , [ Assign
        ( Name "x"
        , Call (Name "+", [ Call (Name "*", [ Hole (Number, "1"); Hole (Number, "2") ]); Num 3 ]) )
    ] )

let candidate3 : program =
  ( String.Map.of_alist_exn []
  , [ Assign (Name "x", Call (Name "+", [ Hole (Number, "1"); Hole (Number, "2") ])) ] )

let candidate4 : program =
  ( String.Map.of_alist_exn []
  , [ Assign (Name "x", Call (Name "+", [ Hole (Number, "1"); Hole (Number, "2") ])) ] )

let unify_raises_error : program -> program -> bool =
 fun reference candidate ->
  match unify reference candidate with
  | exception s -> true
  | _ -> false

let%test_unit "simple hole substitution" =
  [%test_result: substitutions option]
    (unify reference1 candidate1)
    ~expect:(Some (String.Map.of_alist_exn [ ("1", Num 1) ]))

let%test_unit "no substitution possible" =
  [%test_result: substitutions option]
    (unify reference1 candidate2)
    ~expect:None

let%test_unit "2 hole substitutions" =
  [%test_result: substitutions option]
    (unify reference1 candidate3)
    ~expect:(Some (String.Map.of_alist_exn [ ("1", Num 1); ("2", Num 2) ]))

let%test "duplicate hole value" = unify_raises_error reference1 candidate4

let%test_unit "more complex hole substitution" =
  [%test_result: substitutions option]
    (unify reference2 candidate1)
    ~expect:
      (Some
         (String.Map.of_alist_exn [ ("1", Call (Name "*", [ Num 2; Num 3 ])) ]))
