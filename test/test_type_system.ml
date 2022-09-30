open Core
open Lib
open Lang
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location

let test1 = Common.parse_file "programs/test1.lisp"

let%test_unit "test1 well-typed" =
  try Type_system.well_typed test1 with
  | Type_system.IllTyped e -> failwith (Exp.show_single e)

let sigma_list1, gamma_list1, env_list1 =
  Common.parse_file "programs/list1.lisp"

let%test_unit "list1 well-typed" =
  try Type_system.well_typed (sigma_list1, gamma_list1, env_list1) with
  | Type_system.IllTyped e -> failwith (Exp.show_single e)

let%expect_test "list1 extracted foldr for map well-typed" =
  print_endline
    (Typ.show
       (Type_system.infer
          sigma_list1
          gamma_list1
          (Option.value_exn
             (Recursion_scheme.extract_list_foldr
                sigma_list1
                gamma_list1
                env_list1
                "map"))));
  [%expect {| ((Peano -> Peano) -> (ListPeano -> ListPeano)) |}]

let%expect_test "list1 extracted foldr for filter well-typed" =
  print_endline
    (Typ.show
       (Type_system.infer
          sigma_list1
          gamma_list1
          (Option.value_exn
             (Recursion_scheme.extract_list_foldr
                sigma_list1
                gamma_list1
                env_list1
                "filter"))));
  [%expect {| ((Peano -> Bool) -> (ListPeano -> ListPeano)) |}]
