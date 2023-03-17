open Core
open Lang

let reference1 : program =
  ( String.Map.of_alist_exn []
  , [ Assign (Name "x", Call (Name "+", [ Num 1; Num 2 ])) ] )

let candidate1_1 : program =
  ( String.Map.of_alist_exn []
  , [ Assign (Name "x", Call (Name "+", [ Hole "1"; Num 2 ])) ] )

let candidate1_2 : program =
  ( String.Map.of_alist_exn []
  , [ Assign (Name "x", Call (Name "+", [ Num 1; Hole "1" ])) ] )

let candidate1_3 : program =
  ( String.Map.of_alist_exn []
  , [ Assign
        (Name "x", Call (Name "+", [ Call (Name "*", [ Hole "1"; Hole "2" ]) ]))
    ] )

let candidate1_4 : program =
  ( String.Map.of_alist_exn []
  , [ Assign (Name "x", Call (Name "+", [ Hole "1"; Hole "2" ])) ] )
