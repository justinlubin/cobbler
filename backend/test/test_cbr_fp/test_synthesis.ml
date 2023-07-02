open Core
open Cbr_fp
open Lang

let classic = Common.parse_file "programs/classic.lisp"
let list2 = Common.parse_file "programs/list2.lisp"
let poly_mapfilter = Common.parse_file "programs/poly_mapfilter.lisp"

let%test_unit "classic synth 1" =
  let problem = Synthesis.problem_of_definitions classic "main" in
  let expected_solution =
    EApp
      ( EApp (EVar "withDefault", ECtor ("Zero", []))
      , EApp (EApp (EVar "map", EVar "f"), EVar "mx") )
  in
  let actual_solution =
    Synthesis.solve ~use_unification:true ~depth:5 problem
    |> Option.value_exn
    |> snd
    |> Exp.decompose_abs
    |> snd
    |> Exp.clean
  in
  [%test_result: exp] actual_solution ~expect:expected_solution

let%test_unit "list2 mapfilter" =
  let problem = Synthesis.problem_of_definitions list2 "main" in
  let expected_solution =
    EApp
      ( EApp (EVar "map", EVar "f")
      , EApp (EApp (EVar "filter", EVar "pred"), EVar "xs") )
  in
  let actual_solution =
    Synthesis.solve ~use_unification:true ~depth:6 problem
    |> Option.value_exn
    |> snd
    |> Exp.decompose_abs
    |> snd
    |> Exp.clean
  in
  [%test_result: exp] actual_solution ~expect:expected_solution

let%test_unit "poly_mapfilter synthesis" =
  let problem = Synthesis.problem_of_definitions poly_mapfilter "main" in
  let expected_solution =
    EApp
      ( EApp (EVar "map", EVar "f")
      , EApp (EApp (EVar "filter", EVar "pred"), EVar "xs") )
  in
  let actual_solution =
    Synthesis.solve ~use_unification:true ~depth:6 problem
    |> Option.value_exn
    |> snd
    |> Exp.decompose_abs
    |> snd
    |> Exp.clean
  in
  [%test_result: exp] actual_solution ~expect:expected_solution
