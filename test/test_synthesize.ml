open Core
open Lib
open Lang

let%test_unit "classic synth 1" =
  let problem =
    Synthesis.problem_of_definitions (Common.parse_file "programs/classic.lisp")
  in
  let expected_solution =
    EApp
      ( EApp (EVar "withDefault", EVar "zero")
      , EApp (EApp (EVar "map", EVar "f"), EVar "mx") )
  in
  let actual_solution = Synthesis.solve problem |> Option.value_exn in
  [%test_result: exp] actual_solution ~expect:expected_solution
