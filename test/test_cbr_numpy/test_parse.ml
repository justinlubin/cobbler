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

let input_test1 = "x = 3 + 5"
let input_test2 = "y = 2 * 5"

let expected_test1 =
  "((body((Assign(location((start((line 1)(column 0)))(stop((line 1)(column \
   9)))))(targets((Name(location((start((line 1)(column 0)))(stop((line \
   1)(column 1)))))(id x)(ctx Store))))(value(BinOp(location((start((line \
   1)(column 4)))(stop((line 1)(column \
   9)))))(left(Constant(location((start((line 1)(column 4)))(stop((line \
   1)(column 5)))))(value(Integer 3))(kind())))(op \
   Add)(right(Constant(location((start((line 1)(column 8)))(stop((line \
   1)(column 9)))))(value(Integer \
   5))(kind())))))(type_comment()))))(type_ignores()))"

let expected_test2 =
  "((body((Assign(location((start((line 1)(column 0)))(stop((line 1)(column \
   9)))))(targets((Name(location((start((line 1)(column 0)))(stop((line \
   1)(column 1)))))(id y)(ctx Store))))(value(BinOp(location((start((line \
   1)(column 4)))(stop((line 1)(column \
   9)))))(left(Constant(location((start((line 1)(column 4)))(stop((line \
   1)(column 5)))))(value(Integer 2))(kind())))(op \
   Mult)(right(Constant(location((start((line 1)(column 8)))(stop((line \
   1)(column 9)))))(value(Integer \
   5))(kind())))))(type_comment()))))(type_ignores()))"

(*TODO: Un-comment out tests once parsing is working*)
(*let parsed_test1 : string = parse_py input_test1 |> str_of_ast
let parsed_test2 : string = parse_py input_test2 |> str_of_ast

let%test_unit "parse program 1" =
  [%test_result: string] parsed_test1 ~expect:expected_test1

let%test_unit "parse program 2" =
  [%test_result: string] parsed_test2 ~expect:expected_test2*)
