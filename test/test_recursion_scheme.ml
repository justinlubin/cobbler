open Core
open Lib
open Lang
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location

let sigma_list1, gamma_list1, env_list1 =
  Common.parse_file "programs/list1.lisp"

let%expect_test "list1 map" =
  print_endline
    (Exp.show
       (Exp.alpha_normalize
          (Recursion_scheme.extract_list_foldr
             sigma_list1
             gamma_list1
             env_list1
             "map")));
  [%expect
    {| (lambda var0 (Peano -> Peano) (lambda var1 ListPeano (((__list_foldr var1) (lambda var4 Unit (Nil var4))) (lambda var2 (Peano * ListPeano) (lambda var3 ListPeano (Cons ((var0 (fst var2)) , var3))))))) |}]

let%expect_test "list1 filter" =
  print_endline
    (Exp.show
       (Exp.alpha_normalize
          (Recursion_scheme.extract_list_foldr
             sigma_list1
             gamma_list1
             env_list1
             "filter")));
  [%expect
    {| (lambda var0 (Peano -> Bool) (lambda var1 ListPeano (((__list_foldr var1) (lambda var6 Unit (Nil var6))) (lambda var2 (Peano * ListPeano) (lambda var3 ListPeano (match (var0 (fst var2)) (False var4 -> var3) (True var5 -> (Cons ((fst var2) , var3))))))))) |}]
