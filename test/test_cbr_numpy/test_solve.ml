open Cbr_numpy
open Core
open Np_synthesis
open Lang
open Parse
open Util

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
let test_dir = "test_data/programs/"
let s4 : string = In_channel.read_all (test_dir ^ "test_2fors.sexp")
let target4 : program = Parse.program_of_str s4

let target5 : program =
  ( Cbr_numpy.Env.np_env
  , [ Assign
        (PName "z", Call (Name "zeros", [ Call (Name "len", [ Name "x" ]) ]))
    ; For
        ( PName "i"
        , Call (Name "range", [ Call (Name "len", [ Name "x" ]) ])
        , [ Assign
              ( PIndex (PName "z", Name "i")
              , Call
                  ( Name "+"
                  , [ Index (Name "w", Name "i")
                    ; Call
                        ( Name "+"
                        , [ Index (Name "x", Name "i")
                          ; Index (Name "y", Name "i")
                          ] )
                    ] ) )
          ] )
    ; Return (Name "z")
    ] )

let solution1 : program =
  (Cbr_numpy.Env.np_env, [ Return (Call (Name "sum", [ Name "x" ])) ])

let solution2 : program =
  ( Cbr_numpy.Env.np_env
  , [ Return (Call (Name "sum", [ Call (Name "mul", [ Name "x"; Name "y" ]) ]))
    ] )

let solution3 : program =
  (Cbr_numpy.Env.np_env, [ Return (Call (Name "mul", [ Name "x"; Name "y" ])) ])

let solution5 : program =
  ( Cbr_numpy.Env.np_env
  , [ Return
        (Call
           (Name "add", [ Call (Name "add", [ Name "w"; Name "x" ]); Name "y" ]))
    ] )

let no_sol_target : program =
  ( Cbr_numpy.Env.np_env
  , [ Assign (PName "x", Num 0); Return (Call (Name "+", [ Name "x"; Num 1 ])) ]
  )

let egraph_bools = [ true; false ]

let%test_unit "np_solve 1" =
  [%test_result: program list]
    (List.map egraph_bools ~f:(fun use_egraphs ->
         match solve 1 ~debug:false Number target1 use_egraphs with
         | Some p -> p
         | None -> failwith "no solution"))
    ~expect:(repeat solution1 (List.length egraph_bools))

let%test_unit "np_solve 1'" =
  [%test_result: program list]
    (List.map egraph_bools ~f:(fun use_egraphs ->
         match solve 1 Number target1' use_egraphs with
         | Some p -> p
         | None -> failwith "no solution"))
    ~expect:(repeat solution1 (List.length egraph_bools))

let%test_unit "np_solve 2" =
  [%test_result: program list]
    (List.map egraph_bools ~f:(fun use_egraphs ->
         match solve 2 Number target2 use_egraphs with
         | Some p -> p
         | None -> failwith "no solution"))
    ~expect:(repeat solution2 (List.length egraph_bools))

let%test_unit "np_solve 2: not enough depth" =
  [%test_result: program option list]
    (List.map egraph_bools ~f:(fun use_egraphs ->
         solve 1 Number target2 use_egraphs))
    ~expect:(repeat None (List.length egraph_bools))

let%test_unit "np_solve 3" =
  [%test_result: program list]
    (List.map egraph_bools ~f:(fun use_egraphs ->
         match solve 1 ~debug:false Array target3 use_egraphs with
         | Some p -> p
         | None -> failwith "no solution"))
    ~expect:(repeat solution3 (List.length egraph_bools))

let%test_unit "np_solve 3: wrong starting hole type" =
  [%test_result: program option list]
    (List.map egraph_bools ~f:(fun use_egraphs ->
         solve 2 Number target3 use_egraphs))
    ~expect:(repeat None (List.length egraph_bools))

let%test_unit "np_solve no solution" =
  [%test_result: program option list]
    (List.map egraph_bools ~f:(fun use_egraphs ->
         solve 3 Number no_sol_target use_egraphs))
    ~expect:(repeat None (List.length egraph_bools))

let%test_unit "np_solve no solution" =
  [%test_result: program option list]
    (List.map egraph_bools ~f:(fun use_egraphs ->
         solve 1 ~debug:false Number target4 use_egraphs))
    ~expect:(repeat None (List.length egraph_bools))
