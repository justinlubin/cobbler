open Core
open Cbr_fp
open Lang
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location

let test1 = Common.parse_file "programs/test1.lisp"
let test2 = Common.parse_file "programs/test2.lisp"
let long_ctors = Common.parse_file "programs/long_ctors.lisp"

let%test_unit "test1 well-typed" = Type_system.well_typed test1
let%test_unit "test2 well-typed" = Type_system.well_typed test2
let%test_unit "long_ctors well-typed" = Type_system.well_typed long_ctors

let sigma_list1, gamma_list1, env_list1 =
  Common.parse_file "programs/list1.lisp"

let%test_unit "list1 well-typed" =
  Type_system.well_typed (sigma_list1, gamma_list1, env_list1)

let%test_unit "list1 map correct type" =
  Type_system.check
    sigma_list1
    gamma_list1
    (Map.find_exn env_list1 "map")
    (Parse.typ "(((Peano) -> (Peano)) -> ((List (Peano)) -> (List (Peano))))")

let%test_unit "list1 extracted foldr for map well-typed" =
  Type_system.check
    sigma_list1
    gamma_list1
    (Option.value_exn
       (Recursion_scheme.extract_cata sigma_list1 gamma_list1 env_list1 "map"))
    (Parse.typ "(((Peano) -> (Peano)) -> ((List (Peano)) -> (List (Peano))))")

let%test_unit "list1 extracted foldr for filter well-typed" =
  let _ = print_endline "HELLO!" in
  Type_system.check
    sigma_list1
    gamma_list1
    (Option.value_exn
       (Recursion_scheme.extract_cata
          sigma_list1
          gamma_list1
          env_list1
          "filter"))
    (Parse.typ "(((Peano) -> (Bool)) -> ((List (Peano)) -> (List (Peano))))")
