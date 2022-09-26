open Core
open Lib
open Lang

let%test_unit "test1 well-typed" =
  let sigma, gamma, env = Common.parse_file "programs/test1.lisp" in
  try Type_system.well_typed sigma gamma env with
  | Type_system.IllTyped e -> failwith (Exp.show e)

let%test_unit "list1 well-typed" =
  let sigma, gamma, env = Common.parse_file "programs/list1.lisp" in
  try Type_system.well_typed sigma gamma env with
  | Type_system.IllTyped e -> failwith (Exp.show e)
