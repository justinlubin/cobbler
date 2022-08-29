open Core
open Lib
open Lang

(* let%test_unit "classic synth 1" =
  let _, env, exp = Common.parse_file "programs/classic.lisp" in
  let expected_solution =
    EApp
      ( EApp (EVar "withDefault", EVar "zero")
      , EApp (EApp (EVar "map", EVar "f"), EVar "mx") )
  in
  let actual_solution = Synthesize.synthesize env exp |> Option.value_exn in
  [%test_result: exp] ~expect:expected_solution actual_solution *)
