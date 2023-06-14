open Core
open Cbr_fp
open Lang
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location

let sigma_cases, gamma_cases, env_cases =
  Common.parse_file "programs/pull_out_cases.lisp"

let%expect_test "pull out cases 1" =
  let main_cata =
    "main" |> Recursion_scheme.rewrite sigma_cases env_cases |> Option.value_exn
  in
  let fused_main = Fusion.fuse sigma_cases main_cata in
  ignore (Type_system.infer sigma_cases gamma_cases fused_main);
  print_endline (Exp.show_single (Exp.alpha_normalize main_cata));
  print_endline (Exp.show_single (Exp.alpha_normalize fused_main));
  [%expect
    {|
      (lambda var0 (lambda var1 ((cata Maybe (Zero) (lambda var3 var3)) ((cata Maybe (Nothing) (lambda var2 (Just (var0 var2)))) var1))))
      (lambda var0 (lambda var1 ((cata Maybe (Zero) (lambda var2 (var0 var2))) var1))) |}]

let sigma_list2, gamma_list2, env_list2 =
  Common.parse_file "programs/list2.lisp"

let%expect_test "list2 mapmap fusion" =
  let map_foldr =
    "map" |> Recursion_scheme.rewrite sigma_list2 env_list2 |> Option.value_exn
  in
  let mapmap =
    "mapmap"
    |> String.Map.find_exn env_list2
    |> Exp.substitute ("map", map_foldr)
    |> Exp.normalize sigma_list2
  in
  let fused_mapmap = Fusion.fuse sigma_list2 mapmap in
  ignore (Type_system.infer sigma_list2 gamma_list2 fused_mapmap);
  print_endline (Exp.show_single (Exp.alpha_normalize fused_mapmap));
  [%expect
    {| (lambda var0 (lambda var1 (lambda var2 ((cata List (Nil) (lambda var3 (lambda var4 (Cons (var0 (var1 var3)) var4)))) var2)))) |}]

let%expect_test "list2 mapfilter fusion" =
  let map_foldr =
    "map" |> Recursion_scheme.rewrite sigma_list2 env_list2 |> Option.value_exn
  in
  let filter_foldr =
    "filter"
    |> Recursion_scheme.rewrite sigma_list2 env_list2
    |> Option.value_exn
  in
  let mapfilter =
    "mapfilter"
    |> String.Map.find_exn env_list2
    |> Exp.substitute ("map", map_foldr)
    |> Exp.substitute ("filter", filter_foldr)
    |> Exp.normalize sigma_list2
  in
  let fused_mapfilter = Fusion.fuse sigma_list2 mapfilter in
  ignore (Type_system.infer sigma_list2 gamma_list2 fused_mapfilter);
  print_endline (Exp.show_single (Exp.alpha_normalize fused_mapfilter));
  [%expect
    {| (lambda var0 (lambda var1 (lambda var2 ((cata List (Nil) (lambda var3 (lambda var4 ((cata Bool var4 (Cons (var0 var3) var4)) (var1 var3))))) var2)))) |}]
