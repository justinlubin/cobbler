open Cbr_numpy
open Parse
open Core
open Lang

let program_dir = "test_data/programs/"
let expected_env1 = String.Map.of_alist_exn []

let expected_block1 =
  [ Assign (PName "x", Call (Name "+", [ Num 3; Num 5 ]))
  ; Assign
      (PName "y", Call (Name "-", [ Call (Name "+", [ Num 1; Num 2 ]); Num 3 ]))
  ; Assign (PName "z", Call (Name "*", [ Name "y"; Name "x" ]))
  ]

let expected_env2 =
  String.Map.of_alist_exn
    [ ( "dot"
      , ( [ "x"; "y" ]
        , [ Assign (PName "out", Num 0)
          ; For
              ( PName "i"
              , Call (Name "range", [ Call (Name "len", [ Name "x" ]) ])
              , [ Assign
                    ( PName "out"
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
        , [ Assign (PName "out", Num 0)
          ; For
              ( PName "i"
              , Call (Name "range", [ Call (Name "len", [ Name "x" ]) ])
              , [ Assign
                    ( PName "out"
                    , Call (Name "+", [ Name "out"; Index (Name "x", Name "i") ])
                    )
                ] )
          ; Return (Name "out")
          ] ) )
    ; ( "mul"
      , ( [ "x"; "y" ]
        , [ Assign
              ( PName "out"
              , Call (Name "np.zeros", [ Call (Name "len", [ Name "x" ]) ]) )
          ; For
              ( PName "i"
              , Call (Name "range", [ Call (Name "len", [ Name "x" ]) ])
              , [ Assign
                    ( PIndex (PName "out", Name "i")
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
              ( PName "out"
              , Call
                  ( Name "np.zeros"
                  , [ Call (Name "len", [ Name "x" ])
                    ; Call (Name "len", [ Index (Name "y", Num 0) ])
                    ] ) )
          ; For
              ( PName "i"
              , Call (Name "range", [ Call (Name "len", [ Name "x" ]) ])
              , [ For
                    ( PName "j"
                    , Call
                        ( Name "range"
                        , [ Call (Name "len", [ Index (Name "y", Num 0) ]) ] )
                    , [ Assign (PName "dot", Num 0)
                      ; For
                          ( PName "k"
                          , Call
                              (Name "range", [ Call (Name "len", [ Name "y" ]) ])
                          , [ Assign
                                ( PName "dot"
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
                          ( PIndex (PIndex (PName "out", Name "i"), Name "j")
                          , Name "dot" )
                      ] )
                ] )
          ; Return (Name "out")
          ] ) )
    ]

let expected_block3 =
  [ Assign (PName "x", Call (Name "np.zeros", [ Num 2; Num 2 ]))
  ; Assign (PName "y", Call (Name "np.zeros", [ Num 2; Num 2 ]))
  ; Assign (PName "i", Num 1)
  ; Assign (PIndex (PIndex (PName "x", Num 0), Num 0), Num 3)
  ; Assign (PIndex (PIndex (PName "y", Num 1), Num 0), Num 5)
  ; Assign
      ( PIndex
          ( PIndex (PName "x", Call (Name "-", [ Name "i"; Num 1 ]))
          , Call (Name "*", [ Name "i"; Num 1 ]) )
      , Num 4 )
  ; Assign (PName "z", Call (Name "matmul", [ Name "x"; Name "y" ]))
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
