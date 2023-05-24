open Core
open Cbr_fp
open Lang
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location

let sigma_list1, gamma_list1, env_list1 =
  Common.parse_file "programs/list1.lisp"

let sigma_list2, gamma_list2, env_list2 =
  Common.parse_file "programs/list2.lisp"

let%expect_test "list1 map" =
  print_endline
    (Exp.show_single
       (Exp.alpha_normalize
          (Option.value_exn
             (Recursion_scheme.extract_cata
                sigma_list1
                gamma_list1
                env_list1
                "map"))));
  [%expect
    {| (lambda var0 (lambda var1 ((cata List (Nil) (lambda var2 (lambda var3 (Cons (var0 var2) var3)))) var1))) |}]

let%expect_test "list1 filter" =
  print_endline
    (Exp.show_single
       (Exp.alpha_normalize
          (Option.value_exn
             (Recursion_scheme.extract_cata
                sigma_list1
                gamma_list1
                env_list1
                "filter"))));
  [%expect
    {| (lambda var0 (lambda var1 ((cata List (Nil) (lambda var2 (lambda var3 (match (var0 var2) ((False) -> var3) ((True) -> (Cons var2 var3)))))) var1))) |}]

let%expect_test "list2 filter" =
  print_endline
    (Exp.show_single
       (Exp.alpha_normalize
          (Option.value_exn
             (Recursion_scheme.extract_cata
                sigma_list2
                gamma_list2
                env_list2
                "filter"))));
  [%expect
    {| (lambda var0 (lambda var1 ((cata List (Nil) (lambda var2 (lambda var3 (match (var0 var2) ((False) -> var3) ((True) -> (Cons var2 var3)))))) var1))) |}]

let%expect_test "list2 main" =
  print_endline
    (Exp.show_single
       (Exp.alpha_normalize
          (Option.value_exn
             (Recursion_scheme.extract_cata
                sigma_list2
                gamma_list2
                env_list2
                "main"))));
  [%expect
    {| (lambda var0 (lambda var1 (lambda var2 ((cata List (Nil) (lambda var3 (lambda var4 (match (var0 var3) ((False) -> var4) ((True) -> (Cons (var1 var3) var4)))))) var2)))) |}]
