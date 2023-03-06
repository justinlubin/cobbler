open Cbr_numpy
open Parse
open Core
open Lang

let program_dir = "test_data/programs/"
let test1_fname = "test1.py"
let test2_fname = "test2.py"

let sexp_str1 =
  "( () \n\
  \  ((Assign x (Call add (Num 3) (Num 5)))\n\
  \  (Assign y (Call add (Num 1)(Call sub (Num 2) (Num 3))))\n\
  \  (Assign z (Call mul y x))))"

let sexp_str2 =
  "(((dot (x y) \n\
  \         ((Assign out (Call zeros (Call len x))) \n\
  \         (For i (Call range (Call len x)) \n\
  \                ((Assign (Index out i) (Call add (Index out i) (Call mul (Index x i) (Index y \
   i))))\n\
  \        ))))\n\
  \       (sum (x) \n\
  \         ((Assign out (Num 0))\n\
  \         (For i (Call range (Call len x))\n\
  \                ((Assign out (Call add out (Index x i)))))))\n\
  \       (mul (x y)\n\
  \          ((Assign out (Call zeros (Call len x)))\n\
  \           (For i (Call range (Call len x))\n\
  \              ((Assign (Index out i) (Call mul (Index x i) (Index y i)))))\n\
  \          ))) () )"

let expected_env1 = String.Map.of_alist_exn []

let expected_block1 =
  [ Assign (Name "x", Call (Name "add", [ Num 3; Num 5 ]))
  ; Assign
      ( Name "y"
      , Call (Name "add", [ Num 1; Call (Name "sub", [ Num 2; Num 3 ]) ]) )
  ; Assign (Name "z", Call (Name "mul", [ Name "y"; Name "x" ]))
  ]

let expected_env2 =
  String.Map.of_alist_exn
    [ ( "dot"
      , ( [ "x"; "y" ]
        , [ Assign (Name "out", Call (Name "zeros", [ Call (Name "len", [ Name "x"])]))
          ; For
              ( Name "i"
              , Call (Name "range", [ Call (Name "len", [ Name "x" ]) ])
              , [ Assign
                    ( (Index (Name "out", Name "i"))
                    , Call
                        ( Name "add"
                        , [ (Index (Name "out", Name "i"))
                          ; Call
                              ( Name "mul"
                              , [ Index (Name "x", Name "i")
                                ; Index (Name "y", Name "i")
                                ] )
                          ] ) )
                ] )
          ] ) )
    ; ( "sum"
      , ( [ "x" ]
        , [ Assign (Name "out", Num 0)
          ; For
              ( Name "i"
              , Call (Name "range", [ Call (Name "len", [ Name "x" ]) ])
              , [ Assign
                    ( Name "out"
                    , Call
                        ( Name "add"
                        , [ Name "out"
                          ; Index (Name "x", Name "i")
                          ] ) )
                ] )
          ] ) )
    ; ( "mul"
      , ( [ "x"; "y" ]
        , [ Assign
              ( Name "out"
              , Call (Name "zeros", [ Call (Name "len", [ Name "x" ]) ]) )
          ; For
              ( Name "i"
              , Call (Name "range", [ Call (Name "len", [ Name "x" ]) ])
              , [ Assign
                    ( Index (Name "out", Name "i")
                    , Call
                        ( Name "mul"
                        , [ Index (Name "x", Name "i")
                          ; Index (Name "y", Name "i")
                          ] ) )
                ] )
          ] ) )
    ]

let expected_block2 = []

(*TODO: Un-comment out tests once parsing is working*)
let parsed_test1_prog : program = parse_py sexp_str1
let parsed_test2_prog : program = parse_py sexp_str2

let%test_unit "parse program 1" =
  [%test_result: string]
    (str_of_program parsed_test1_prog)
    ~expect:(str_of_program (expected_env1, expected_block1))

let%test_unit "parse program 2" =
  [%test_result: string] (str_of_program parsed_test2_prog)
    ~expect:(str_of_program (expected_env2, expected_block2))
