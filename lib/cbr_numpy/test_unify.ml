open Core
open Lang

let prog1 : program =
  ( String.Map.of_alist_exn []
  , [ Assign (Name "x", Call (Name "Add", [ Num 1; Num 2 ])) ] )

let prog2 : program =
  ( String.Map.of_alist_exn []
  , [ Assign (Name "x", Call (Name "Add", [ Hole "1"; Num 2 ])) ] )

let prog3 : program =
  ( String.Map.of_alist_exn []
  , [ Assign (Name "x", Call (Name "Add", [ Num 1; Hole "1" ])) ] )
