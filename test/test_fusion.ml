open Core
open Lib
open Lang

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
  Common.parse_file "programs/list1.lisp"

let%test_unit "list2 map map fusion" =
  let map_foldr =
    Recursion_scheme.extract_list_foldr sigma_list2 gamma_list2 env_list2 "map"
  in
  failwith "TODO"
