open Cbr_numpy
open Parse
open Core
open Lang

let program_dir = "test_data/programs/"
let expected_env1 = String.Map.of_alist_exn []

let expected_block1 =
  [ Assign (Name "x", Call (Name "+", [ Num 3; Num 5 ]))
  ; Assign
      (Name "y", Call (Name "-", [ Call (Name "+", [ Num 1; Num 2 ]); Num 3 ]))
  ; Assign (Name "z", Call (Name "*", [ Name "y"; Name "x" ]))
  ]

let expected_env2 =
  String.Map.of_alist_exn
    [ ( "dot"
      , ( [ "x"; "y" ]
        , [ Assign (Name "out", Num 0)
          ; For
              ( "i"
              , Call (Name "range", [ Call (Name "len", [ Name "x" ]) ])
              , [ Assign
                    ( Name "out"
                    , Call
                        ( Name "+"
                        , [ Name "out"
                          ; Call
                              ( Name "*"
                              , [ Index (Name "x", Name "i")
                                ; Index (Name "y", Name "i")
                                ] )
                          ] ) )
                ] )
          ; Return (Name "out")
          ] ) )
    ; ( "sum"
      , ( [ "x" ]
        , [ Assign (Name "out", Num 0)
          ; For
              ( "i"
              , Call (Name "range", [ Call (Name "len", [ Name "x" ]) ])
              , [ Assign
                    ( Name "out"
                    , Call (Name "+", [ Name "out"; Index (Name "x", Name "i") ])
                    )
                ] )
          ; Return (Name "out")
          ] ) )
    ; ( "mul"
      , ( [ "x"; "y" ]
        , [ Assign
              ( Name "out"
              , Call (Name "np_zeros", [ Call (Name "len", [ Name "x" ]) ]) )
          ; For
              ( "i"
              , Call (Name "range", [ Call (Name "len", [ Name "x" ]) ])
              , [ Assign
                    ( Index (Name "out", Name "i")
                    , Call
                        ( Name "*"
                        , [ Index (Name "x", Name "i")
                          ; Index (Name "y", Name "i")
                          ] ) )
                ] )
          ; Return (Name "out")
          ] ) )
    ]

let expected_block2 = []

let expected_env3 =
  String.Map.of_alist_exn
    [ ( "matmul"
      , ( [ "x"; "y" ]
        , [ Assign
              ( Name "out"
              , Call
                  ( Name "np_zeros"
                  , [ Call (Name "len", [ Name "x" ])
                    ; Call (Name "len", [ Index (Name "y", Num 0) ])
                    ] ) )
          ; For
              ( "i"
              , Call (Name "range", [ Call (Name "len", [ Name "x" ]) ])
              , [ For
                    ( "j"
                    , Call
                        ( Name "range"
                        , [ Call (Name "len", [ Index (Name "y", Num 0) ]) ] )
                    , [ Assign (Name "dot", Num 0)
                      ; For
                          ( "k"
                          , Call
                              (Name "range", [ Call (Name "len", [ Name "y" ]) ])
                          , [ Assign
                                ( Name "dot"
                                , Call
                                    ( Name "+"
                                    , [ Name "dot"
                                      ; Call
                                          ( Name "*"
                                          , [ Index
                                                ( Index (Name "x", Name "i")
                                                , Name "k" )
                                            ; Index
                                                ( Index (Name "y", Name "k")
                                                , Name "j" )
                                            ] )
                                      ] ) )
                            ] )
                      ; Assign
                          ( Index (Index (Name "out", Name "i"), Name "j")
                          , Name "dot" )
                      ] )
                ] )
          ; Return (Name "out")
          ] ) )
    ]

let expected_block3 =
  [ Assign (Name "x", Call (Name "np_zeros", [ Num 2; Num 2 ]))
  ; Assign (Name "y", Call (Name "np_zeros", [ Num 2; Num 2 ]))
  ; Assign (Name "i", Num 1)
  ; Assign (Index (Index (Name "x", Num 0), Num 0), Num 3)
  ; Assign (Index (Index (Name "y", Num 1), Num 0), Num 5)
  ; Assign
      ( Index
          ( Index (Name "x", Call (Name "-", [ Name "i"; Num 1 ]))
          , Call (Name "*", [ Name "i"; Num 1 ]) )
      , Num 4 )
  ; Assign (Name "z", Call (Name "matmul", [ Name "x"; Name "y" ]))
  ]

let expected_progs : program list =
  [ (expected_env1, expected_block1)
  ; (expected_env2, expected_block2)
  ; (expected_env3, expected_block3)
  ]

let test_fnames = [ "test1.sexp"; "test2.sexp"; "test3.sexp" ]

let parsed_progs : program list =
  List.map test_fnames ~f:(fun fname ->
      program_dir ^ fname
      |> In_channel.with_file ~f:(fun file ->
             program_of_str (In_channel.input_all file)))

let%test_unit "parse program 1" =
  [%test_result: program]
    (List.nth_exn parsed_progs 0)
    ~expect:(List.nth_exn expected_progs 0)

let%test_unit "parse program 2" =
  [%test_result: program]
    (List.nth_exn parsed_progs 1)
    ~expect:(List.nth_exn expected_progs 1)

let%test_unit "parse program 3" =
  [%test_result: program]
    (List.nth_exn parsed_progs 2)
    ~expect:(List.nth_exn expected_progs 2)
