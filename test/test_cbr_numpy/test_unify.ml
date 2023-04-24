open Core
open Cbr_numpy
open Parse
open Unification
open Lang
open Util

let reference1 : program =
  (String.Map.empty, [ Assign (PName "x", Call (Name "+", [ Num 1; Num 2 ])) ])

let reference2 : program =
  ( String.Map.empty
  , [ Assign
        ( PName "x"
        , Call (Name "+", [ Call (Name "*", [ Num 2; Num 3 ]); Num 2 ]) )
    ] )

let reference4 : program =
  (String.Map.empty, [ Assign (PName "x", Call (Name "+", [ Num 2; Num 2 ])) ])

let reference4' : program =
  ( String.Map.empty
  , [ Assign
        ( PName "x"
        , Call
            ( Name "+"
            , [ Index (Name "a", Name "i"); Index (Name "a", Name "i") ] ) )
    ] )

let reference4'' : program =
  (String.Map.empty, [ Assign (PName "x", Call (Name "+", [ Num 1; Num 2 ])) ])

let candidate1 : program =
  ( String.Map.empty
  , [ Assign (PName "x", Call (Name "+", [ Hole (Number, "1"); Num 2 ])) ] )

let candidate2 : program =
  ( String.Map.empty
  , [ Assign
        ( PName "x"
        , Call
            ( Name "+"
            , [ Call (Name "*", [ Hole (Number, "1"); Hole (Number, "2") ])
              ; Num 3
              ] ) )
    ] )

let candidate3 : program =
  ( String.Map.empty
  , [ Assign
        (PName "x", Call (Name "+", [ Hole (Number, "1"); Hole (Number, "2") ]))
    ] )

let candidate4 : program =
  ( String.Map.empty
  , [ Assign
        (PName "x", Call (Name "+", [ Hole (Number, "x"); Hole (Number, "x") ]))
    ] )

let unify_raises_error : program -> program -> bool =
 fun reference candidate ->
  match unify_naive ~target:reference ~pattern:candidate () with
  | exception s -> true
  | _ -> false

let unify_funcs = [ unify_egraph; unify_naive ]

let%test_unit "simple hole substitution" =
  let expect = Some (String.Map.of_alist_exn [ ("1", Num 1) ]) in
  [%test_result: substitutions option list]
    (List.map unify_funcs ~f:(fun unify ->
         unify ~target:reference1 ~pattern:candidate1 ()))
    ~expect:(repeat expect (List.length unify_funcs))

let%test_unit "no substitution possible" =
  [%test_result: substitutions option list]
    (List.map unify_funcs ~f:(fun unify ->
         unify ~debug:false ~target:reference1 ~pattern:candidate2 ()))
    ~expect:(repeat None (List.length unify_funcs))

let%test_unit "2 hole substitutions" =
  let expect = Some (String.Map.of_alist_exn [ ("1", Num 1); ("2", Num 2) ]) in
  [%test_result: substitutions option list]
    (List.map unify_funcs ~f:(fun unify ->
         unify ~debug:false ~target:reference1 ~pattern:candidate3 ()))
    ~expect:(repeat expect (List.length unify_funcs))

let%test_unit "more complex hole substitution" =
  let expect =
    Some (String.Map.of_alist_exn [ ("1", Call (Name "*", [ Num 2; Num 3 ])) ])
  in
  [%test_result: substitutions option]
    (unify_naive ~debug:false ~target:reference2 ~pattern:candidate1 ())
    ~expect

let%test_unit "duplicate hole substitution" =
  let expect = Some (String.Map.of_alist_exn [ ("x", Num 2) ]) in
  [%test_result: substitutions option list]
    (List.map unify_funcs ~f:(fun unify ->
         unify ~debug:false ~target:reference4 ~pattern:candidate4 ()))
    ~expect:(repeat expect (List.length unify_funcs))

let%test_unit "duplicate hole substitution with complex expr" =
  let expect =
    Some (String.Map.of_alist_exn [ ("x", Index (Name "a", Name "i")) ])
  in
  [%test_result: substitutions option list]
    (List.map unify_funcs ~f:(fun unify ->
         unify ~debug:false ~target:reference4' ~pattern:candidate4 ()))
    ~expect:(repeat expect (List.length unify_funcs))

let%test_unit "duplicate hole substitution fail for unmatching exprs" =
  [%test_result: substitutions option list]
    (List.map unify_funcs ~f:(fun unify ->
         unify ~debug:false ~target:reference4'' ~pattern:candidate4 ()))
    ~expect:(repeat None (List.length unify_funcs))
