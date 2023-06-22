open Cbr_numpy
open Core
open Np_synthesis
open Lang
open Parse
open Util

let cap : string -> string -> expr =
 fun y x -> Call (Name "sliceUntil", [ Name y; Call (Name "len", [ Name x ]) ])

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
  \    ((Assign z (Call np.zeros (Call len x)))\n\
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
        (PName "z", Call (Name "np.zeros", [ Call (Name "len", [ Name "x" ]) ]))
    ; For
        ( PName "i"
        , Call (Name "range", [ Call (Name "len", [ Name "x" ]) ])
        , [ Assign
              ( PIndex (PName "z", Name "i")
              , Call
                  ( Name "*"
                  , [ Index (Name "w", Name "i")
                    ; Call
                        ( Name "*"
                        , [ Index (Name "x", Name "i")
                          ; Index (Name "y", Name "i")
                          ] )
                    ] ) )
          ] )
    ; Return (Name "z")
    ] )

let target6 : program =
  ( Cbr_numpy.Env.np_env
  , [ Assign
        ( PName "out"
        , Call (Name "np.zeros", [ Call (Name "len", [ Name "x" ]) ]) )
    ; For
        ( PName "i"
        , Call (Name "range", [ Call (Name "len", [ Name "x" ]) ])
        , [ Assign (PName "s", Num 0)
          ; For
              ( PName "j"
              , Call (Name "range", [ Call (Name "len", [ Name "x" ]) ])
              , [ Assign
                    ( PName "s"
                    , Call
                        ( Name "+"
                        , [ Name "s"
                          ; Call
                              ( Name "*"
                              , [ Index (Name "x", Name "j")
                                ; Index
                                    ( Name "y"
                                    , Call (Name "-", [ Name "j"; Name "i" ]) )
                                ] )
                          ] ) )
                ] )
          ; Assign (PIndex (PName "out", Name "i"), Name "s")
          ] )
    ; Return (Name "out")
    ] )

let target7 : program =
  ( Cbr_numpy.Env.np_env
  , [ Assign
        ( PName "out"
        , Call (Name "np.zeros", [ Call (Name "len", [ Name "x" ]) ]) )
    ; For
        ( PName "i"
        , Call (Name "range", [ Call (Name "len", [ Name "x" ]) ])
        , [ If
              ( Call (Name ">", [ Index (Name "x", Name "i"); Num 0 ])
              , [ Assign (PIndex (PName "out", Name "i"), Num 1) ]
              , [ Assign (PIndex (PName "out", Name "i"), Num (-1)) ] )
          ] )
    ; Return (Name "out")
    ] )

let target8 : program =
  ( Cbr_numpy.Env.np_env
  , [ Assign
        ( PName "y"
        , Call
            ( Name "np.zeros"
            , [ Call
                  ( Name "+"
                  , [ Call
                        ( Name "-"
                        , [ Call (Name "len", [ Name "x" ])
                          ; Name "window_size"
                          ] )
                    ; Num 1
                    ] )
              ] ) )
    ; For
        ( PName "i"
        , Call
            ( Name "range"
            , [ Call
                  ( Name "+"
                  , [ Call
                        ( Name "-"
                        , [ Call (Name "len", [ Name "x" ])
                          ; Name "window_size"
                          ] )
                    ; Num 1
                    ] )
              ] )
        , [ Assign (PName "s", Num 0)
          ; For
              ( PName "j"
              , Call (Name "range", [ Name "window_size" ])
              , [ Assign
                    ( PName "s"
                    , Call
                        ( Name "+"
                        , [ Name "s"
                          ; Index
                              (Name "x", Call (Name "+", [ Name "i"; Name "j" ]))
                          ] ) )
                ] )
          ; Assign (PIndex (PName "y", Name "i"), Name "s")
          ] )
    ; Return (Name "y")
    ] )

let solution1 : program =
  (Cbr_numpy.Env.np_env, [ Return (Call (Name "np.sum", [ Name "x" ])) ])

let solution2 : program =
  ( Cbr_numpy.Env.np_env
  , [ Return
        (Call
           ( Name "np.sum"
           , [ Call (Name "np.multiply", [ Name "x"; cap "y" "x" ]) ] ))
    ] )

let solution3 : program =
  ( Cbr_numpy.Env.np_env
  , [ Return (Call (Name "np.multiply", [ Name "x"; cap "y" "x" ])) ] )

let solution5 : program =
  ( Cbr_numpy.Env.np_env
  , [ Return
        (Call
           ( Name "np.multiply"
           , [ Call (Name "np.multiply", [ Name "x"; cap "y" "x" ])
             ; cap "w" "x"
             ] ))
    ] )

let no_sol_target : program =
  ( Cbr_numpy.Env.np_env
  , [ Assign (PName "x", Num 0); Return (Call (Name "+", [ Name "x"; Num 1 ])) ]
  )

let solution7 : program =
  ( Cbr_numpy.Env.np_env
  , [ Return
        (Call
           ( Name "np.where"
           , [ Call
                 ( Name "np.greater"
                 , [ Name "x"; Call (Name "broadcast", [ Num 0 ]) ] )
             ; Call (Name "broadcast", [ Num 1 ])
             ; Call (Name "broadcast", [ Num (-1) ])
             ] ))
    ] )

let solution8 : program =
  ( Env.np_env
  , [ Return
        (Call
           ( Name "np.convolve_valid"
           , [ Name "x"; Call (Name "np.full", [ Name "window_size"; Num 1 ]) ]
           ))
    ] )

let egraph_bools = [ true; false ]

let%test_unit "np_solve 1" =
  [%test_result: program list]
    (List.map egraph_bools ~f:(fun use_egraphs ->
         match solve 1 ~debug:false target1 use_egraphs with
         | Some p -> p
         | None -> failwith "no solution"))
    ~expect:(repeat solution1 (List.length egraph_bools))

let%test_unit "np_solve 1'" =
  [%test_result: program list]
    (List.map egraph_bools ~f:(fun use_egraphs ->
         match solve 1 target1' use_egraphs with
         | Some p -> p
         | None -> failwith "no solution"))
    ~expect:(repeat solution1 (List.length egraph_bools))

let%test_unit "np_solve 2" =
  [%test_result: program list]
    (List.map egraph_bools ~f:(fun use_egraphs ->
         match solve 2 target2 use_egraphs with
         | Some p -> p
         | None -> failwith "no solution"))
    ~expect:(repeat solution2 (List.length egraph_bools))

let%test_unit "np_solve 2: not enough depth" =
  [%test_result: program option list]
    (List.map egraph_bools ~f:(fun use_egraphs -> solve 1 target2 use_egraphs))
    ~expect:(repeat None (List.length egraph_bools))

let%test_unit "np_solve 3" =
  [%test_result: program list]
    (List.map egraph_bools ~f:(fun use_egraphs ->
         match solve 1 ~debug:false target3 use_egraphs with
         | Some p -> p
         | None -> failwith "no solution"))
    ~expect:(repeat solution3 (List.length egraph_bools))

let%test_unit "np_solve no solution" =
  [%test_result: program option list]
    (List.map egraph_bools ~f:(fun use_egraphs ->
         solve 3 no_sol_target use_egraphs))
    ~expect:(repeat None (List.length egraph_bools))

let%test_unit "np_solve no solution" =
  [%test_result: program option list]
    (List.map egraph_bools ~f:(fun use_egraphs ->
         solve 1 ~debug:false target4 use_egraphs))
    ~expect:(repeat None (List.length egraph_bools))

let%test_unit "np_solve 2 muls" =
  [%test_result: program option list]
    (List.map egraph_bools ~f:(fun use_egraphs ->
         solve 2 ~debug:false target5 use_egraphs))
    ~expect:[ Some solution5; None ]

let%test_unit "np_solve where" =
  [%test_result: program option list]
    (List.map egraph_bools ~f:(fun use_egraphs ->
         solve 2 ~debug:false target7 use_egraphs))
    ~expect:[ Some solution7; None ]

let%test_unit "np_solve rolling sum" =
  [%test_result: program option list]
    (List.map egraph_bools ~f:(fun use_egraphs ->
         solve 2 ~debug:false target8 use_egraphs))
    ~expect:[ Some solution8; None ]
