open Cbr_numpy
open Core
open Np_synthesis
open Lang
open Parse

let target1 : program =
  ( Cbr_numpy.Env.np_env
  , [ Assign (PName "c", Num 0)
    ; For
        ( PName "i"
        , Call (Name "range", [ Call (Name "len", [ Name "x" ]) ])
        , [ Assign
              ( PName "c"
              , Call (Name "+", [ Name "c"; Index (Name "x", Name "i") ]) )
          ] )
    ; Return (Name "c")
    ] )

let s1 : string =
  "( ()\n\
  \    ((Assign c (Num 0)) \n\
  \    (For i (Call range (Call len x)) ((Assign c (Call + c (Index x i)))))\n\
  \    (Return c)))"

let target1' : program = Parse.program_of_str s1

let target2 : program =
  ( Cbr_numpy.Env.np_env
  , [ Assign (PName "c", Num 0)
    ; For
        ( PName "j"
        , Call (Name "range", [ Call (Name "len", [ Name "x" ]) ])
        , [ Assign
              ( PName "c"
              , Call
                  ( Name "+"
                  , [ Name "c"
                    ; Call
                        ( Name "*"
                        , [ Index (Name "x", Name "j")
                          ; Index (Name "y", Name "j")
                          ] )
                    ] ) )
          ] )
    ; Return (Name "c")
    ] )

let s3 : string =
  "( ()\n\
  \    ((Assign z (Call zeros (Call len x)))\n\
  \    (For i (Call range (Call len x)) ((Assign (Index z i) (Call * (Index x \
   i) (Index y i)))))\n\
  \    (Return z)\n\
  \    )\n\
  \  )"

let target3 : program = Parse.program_of_str s3

let solution1 : program =
  (Cbr_numpy.Env.np_env, [ Return (Call (Name "sum", [ Name "x" ])) ])

let solution2 : program =
  ( Cbr_numpy.Env.np_env
  , [ Return (Call (Name "sum", [ Call (Name "mul", [ Name "x"; Name "y" ]) ]))
    ] )

let solution3 : program =
  (Cbr_numpy.Env.np_env, [ Return (Call (Name "mul", [ Name "x"; Name "y" ])) ])

let no_sol_target : program =
  ( Cbr_numpy.Env.np_env
  , [ Assign (PName "x", Num 0); Return (Call (Name "+", [ Name "x"; Num 1 ])) ]
  )

let%test_unit "np_solve 1" =
  [%test_result: program]
    (match solve 1 Number target1 with
    | Some p -> p
    | None -> failwith "no solution")
    ~expect:solution1

let%test_unit "np_solve 1'" =
  [%test_result: program]
    (match solve 1 Number target1' with
    | Some p -> p
    | None -> failwith "no solution")
    ~expect:solution1

let%test_unit "np_solve 2" =
  [%test_result: program]
    (match solve 2 Number target2 with
    | Some p -> p
    | None -> failwith "no solution")
    ~expect:solution2

let%test_unit "np_solve 2: not enough depth" =
  [%test_result: program option] (solve 1 Number target2) ~expect:None

let%test_unit "np_solve 3" =
  [%test_result: program]
    (match solve 1 Array target3 with
    | Some p -> p
    | None -> failwith "no solution")
    ~expect:solution3

let%test_unit "np_solve 3: wrong starting hole type" =
  [%test_result: program option] (solve 2 Number target3) ~expect:None

let%test_unit "np_solve no solution" =
  [%test_result: program option] (solve 3 Number no_sol_target) ~expect:None
