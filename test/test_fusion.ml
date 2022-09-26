open Core
open Lib
open Lang

let%test_unit "pull out cases 1" =
  [%test_eq: exp]
    (Fusion.pull_out_cases
       (EMatch
          ( EMatch
              ( EVar "mx"
              , [ ("Nothing", ("n1", ECtor ("Nothing", EVar "n1")))
                ; ("Just", ("x", ECtor ("Just", EApp (EVar "f", EVar "x"))))
                ] )
          , [ ("Nothing", ("n2", EVar "zero")); ("Just", ("y", EVar "y")) ] )))
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
