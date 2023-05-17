open Core
open Cbr_fp
open Lang
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location

let%test_unit "pull out cases 1" =
  [%test_result: exp]
    (Fusion.pull_out_cases
       (EMatch
          ( EMatch
              ( EVar "mx"
              , [ ("Nothing", ("n1", ECtor ("Nothing", EVar "n1")))
                ; ("Just", ("x", ECtor ("Just", EApp (EVar "f", EVar "x"))))
                ] )
          , [ ("Nothing", ("n2", EVar "zero")); ("Just", ("y", EVar "y")) ] )))
    ~expect:
      (EMatch
         ( EVar "mx"
         , [ ( "Nothing"
             , ( "n1"
               , EMatch
                   ( ECtor ("Nothing", EVar "n1")
                   , [ ("Nothing", ("n2", EVar "zero"))
                     ; ("Just", ("y", EVar "y"))
                     ] ) ) )
           ; ( "Just"
             , ( "x"
               , EMatch
                   ( ECtor ("Just", EApp (EVar "f", EVar "x"))
                   , [ ("Nothing", ("n2", EVar "zero"))
                     ; ("Just", ("y", EVar "y"))
                     ] ) ) )
           ] ))

let sigma_list2, gamma_list2, env_list2 =
  Common.parse_file "programs/list2.lisp"

let%expect_test "list2 mapmap fusion" =
  let map_foldr =
    "map"
    |> Recursion_scheme.extract_list_foldr sigma_list2 gamma_list2 env_list2
    |> Option.value_exn
  in
  let mapmap =
    "mapmap"
    |> String.Map.find_exn env_list2
    |> Exp.substitute ("map", map_foldr)
    |> Exp.normalize
  in
  let fused_mapmap = Fusion.fuse sigma_list2 gamma_list2 mapmap in
  ignore (Type_system.infer sigma_list2 gamma_list2 fused_mapmap);
  print_endline (Exp.show_single (Exp.alpha_normalize fused_mapmap));
  [%expect
    {| (lambda var0 ((Peano) -> (Peano)) (lambda var1 ((Peano) -> (Peano)) (lambda var2 (ListPeano) ((list_foldr (Nil ()) (lambda var3 ((Peano) * (ListPeano)) (Cons ((var0 (var1 (fst var3))) , (snd var3))))) var2)))) |}]

let%expect_test "list2 mapfilter fusion" =
  let map_foldr =
    "map"
    |> Recursion_scheme.extract_list_foldr sigma_list2 gamma_list2 env_list2
    |> Option.value_exn
  in
  let filter_foldr =
    "filter"
    |> Recursion_scheme.extract_list_foldr sigma_list2 gamma_list2 env_list2
    |> Option.value_exn
  in
  let mapfilter =
    "mapfilter"
    |> String.Map.find_exn env_list2
    |> Exp.substitute ("map", map_foldr)
    |> Exp.substitute ("filter", filter_foldr)
    |> Exp.normalize
  in
  let fused_mapfilter = Fusion.fuse sigma_list2 gamma_list2 mapfilter in
  ignore (Type_system.infer sigma_list2 gamma_list2 fused_mapfilter);
  print_endline (Exp.show_single (Exp.alpha_normalize fused_mapfilter));
  [%expect
    {| (lambda var0 ((Peano) -> (Peano)) (lambda var1 ((Peano) -> (Bool)) (lambda var2 (ListPeano) ((list_foldr (Nil ()) (lambda var3 ((Peano) * (ListPeano)) (match (var1 (fst var3)) (False var4 -> (snd var3)) (True var5 -> (Cons ((var0 (fst var3)) , (snd var3))))))) var2)))) |}]
