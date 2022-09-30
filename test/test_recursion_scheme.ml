open Core
open Lib
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
    (Exp.show
       (Exp.alpha_normalize
          (Recursion_scheme.extract_list_foldr
             sigma_list1
             gamma_list1
             env_list1
             "map")));
  [%expect
    {| (lambda var0 (Peano -> Peano) (lambda var1 ListPeano ((list_foldr (Nil ()) (lambda var2 (Peano * ListPeano) (Cons ((var0 (fst var2)) , (snd var2))))) var1))) |}]

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
    {| (lambda var0 (Peano -> Bool) (lambda var1 ListPeano ((list_foldr (Nil ()) (lambda var2 (Peano * ListPeano) (match (var0 (fst var2)) (False var3 -> (snd var2)) (True var4 -> (Cons ((fst var2) , (snd var2))))))) var1))) |}]

let%expect_test "list2 main" =
  print_endline
    (Exp.show
       (Exp.alpha_normalize
          (Recursion_scheme.extract_list_foldr
             sigma_list2
             gamma_list2
             env_list2
             "main")));
  [%expect
    {| (lambda var0 (Peano -> Bool) (lambda var1 (Peano -> Peano) (lambda var2 ListPeano ((list_foldr (Nil ()) (lambda var3 (Peano * ListPeano) (Cons ((var0 (var1 (fst var3))) , (snd var3))))) var2)))) |}]
