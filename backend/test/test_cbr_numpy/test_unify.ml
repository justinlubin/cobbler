open Core
open Cbr_numpy
open Parse
open Unification
open Lang
open Util

let reference1 : program =
  (String.Map.empty, [ Assign (PName "x", Call (Name "+", [ Num 1; Num 2 ])) ])

let reference2 : program =
  ( String.Map.empty
  , [ Assign
        ( PName "x"
        , Call (Name "+", [ Call (Name "*", [ Num 2; Num 3 ]); Num 2 ]) )
    ] )

let reference4 : program =
  (String.Map.empty, [ Assign (PName "x", Call (Name "+", [ Num 2; Num 2 ])) ])

let reference4' : program =
  ( String.Map.empty
  , [ Assign
        ( PName "x"
        , Call
            ( Name "+"
            , [ Index (Name "a", Name "i"); Index (Name "a", Name "i") ] ) )
    ] )

let reference4'' : program =
  (String.Map.empty, [ Assign (PName "x", Call (Name "+", [ Num 1; Num 2 ])) ])

let reference5 : program =
  ( String.Map.empty
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

let reference6 : program = (String.Map.empty, [ Assign (PName "c", Name "c") ])

let reference7 : program =
  (String.Map.empty, [ Return (Index (Name "x", Num 3)) ])

let s8 : string =
  "( ()\n\
  \    ((Assign z (Call np.zeros (Call len x)))\n\
  \    (For i (Call range (Call len x)) ((Assign (Index z i) (Call * (Index x \
   i) (Index y i)))))\n\
  \    (Return z))\n\
  \  )"

let reference8 : program = program_of_str s8

let reference9 : program =
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

let reference10 : program =
  Partial_eval.partial_eval_program
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
          , Call (Name "range", [ Call (Name "len", [ Name "y" ]) ])
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
                                ( Name "x"
                                , Call (Name "+", [ Name "i"; Name "j" ]) )
                            ] ) )
                  ] )
            ; Assign (PIndex (PName "y", Name "i"), Name "s")
            ] )
      ; Return (Name "y")
      ] )

let candidate1 : program =
  ( String.Map.empty
  , [ Assign (PName "x", Call (Name "+", [ Hole (Number, "1"); Num 2 ])) ] )

let candidate2 : program =
  ( String.Map.empty
  , [ Assign
        ( PName "x"
        , Call
            ( Name "+"
            , [ Call (Name "*", [ Hole (Number, "1"); Hole (Number, "2") ])
              ; Num 3
              ] ) )
    ] )

let candidate3 : program =
  ( String.Map.empty
  , [ Assign
        (PName "x", Call (Name "+", [ Hole (Number, "1"); Hole (Number, "2") ]))
    ] )

let candidate4 : program =
  ( String.Map.empty
  , [ Assign
        (PName "x", Call (Name "+", [ Hole (Number, "x"); Hole (Number, "x") ]))
    ] )

let candidate5 : program =
  ( String.Map.empty
  , [ Assign (PHole (Number, "a"), Num 0)
    ; For
        ( PHole (Number, "b")
        , Call (Name "range", [ Call (Name "len", [ Hole (Number, "c") ]) ])
        , [ Assign
              ( PHole (Number, "a")
              , Call
                  ( Name "+"
                  , [ Hole (Number, "a")
                    ; Index (Hole (Number, "c"), Hole (Number, "b"))
                    ] ) )
          ] )
    ; Return (Hole (Number, "a"))
    ] )

let candidate6 : program =
  (String.Map.empty, [ Assign (PHole (Number, "a"), Hole (Number, "a")) ])

let candidate7 : program =
  (String.Map.empty, [ Return (Index (Hole (Number, "a"), Num 3)) ])

let candidate8 : program =
  ( String.Map.empty
  , [ Assign
        ( PHole (Array, "a")
        , Call (Name "np.zeros", [ Call (Name "len", [ Hole (Array, "b") ]) ])
        )
    ; For
        ( PHole (Number, "c")
        , Call (Name "range", [ Call (Name "len", [ Hole (Array, "b") ]) ])
        , [ Assign
              ( PIndex (PHole (Array, "a"), Hole (Number, "c"))
              , Call
                  ( Name "*"
                  , [ Index (Hole (Array, "b"), Hole (Number, "c"))
                    ; Index (Hole (Array, "d"), Hole (Number, "c"))
                    ] ) )
          ] )
    ; Return (Hole (Array, "a"))
    ] )

let candidate8' : program =
  ( String.Map.empty
  , [ Assign
        ( PHole (Array, "a")
        , Call (Name "np.zeros", [ Call (Name "len", [ Hole (Array, "b") ]) ])
        )
    ; For
        ( PHole (Number, "c")
        , Call (Name "range", [ Call (Name "len", [ Hole (Array, "b") ]) ])
        , [ Assign
              ( PIndex (PHole (Array, "a"), Hole (Number, "c"))
              , Call
                  ( Name "*"
                  , [ Index (Hole (Array, "d"), Hole (Number, "c"))
                    ; Index (Hole (Array, "b"), Hole (Number, "c"))
                    ] ) )
          ] )
    ; Return (Hole (Array, "a"))
    ] )

let candidate9 : program =
  ( Cbr_numpy.Env.np_env
  , [ Assign
        ( PHole (Array, "out")
        , Call (Name "np.zeros", [ Call (Name "len", [ Hole (Array, "x") ]) ])
        )
    ; For
        ( PHole (Array, "i")
        , Call (Name "range", [ Call (Name "len", [ Hole (Array, "x") ]) ])
        , [ If
              ( Call
                  ( Name ">"
                  , [ Index (Hole (Array, "cond"), Name "i")
                    ; Index (Hole (Array, "cond2"), Name "i")
                    ] )
              , [ Assign (PIndex (PHole (Array, "out"), Name "i"), Num 1) ]
              , [ Assign (PIndex (PHole (Array, "out"), Name "i"), Num (-1)) ]
              )
          ] )
    ; Return (Hole (Array, "out"))
    ] )

let candidate9' : program =
  ( Cbr_numpy.Env.np_env
  , [ Return
        (Call
           ( Name "np.where"
           , [ Call (Name "np.greater", [ Hole (Array, "x"); Hole (Array, "z") ])
             ; Hole (Number, "pos")
             ; Hole (Number, "neg")
             ] ))
    ] )

let candidate10 : program =
  ( Env.np_env
  , [ Return
        (Call
           ( Name "np.convolve_valid"
           , [ Hole (Array, "x")
             ; Call (Name "np.full", [ Hole (Number, "w"); Num 1 ])
             ] ))
    ] )
  |> Inline.inline_program
  |> Partial_eval.partial_eval_program

let unify_raises_error : program -> program -> bool =
 fun reference candidate ->
  match unify_naive ~target:reference ~pattern:candidate () with
  | exception s -> true
  | _ -> false

let unify_funcs = [ unify_egraph_full; unify_naive ]

let%test_unit "simple hole substitution" =
  let expect = Some (String.Map.of_alist_exn [ ("1", Num 1) ]) in
  [%test_result: substitutions option list]
    (List.map unify_funcs ~f:(fun unify ->
         unify ~debug:false ~target:reference1 ~pattern:candidate1 ()))
    ~expect:(repeat (List.length unify_funcs) expect)

let%test_unit "no substitution possible" =
  [%test_result: substitutions option list]
    (List.map unify_funcs ~f:(fun unify ->
         unify ~debug:false ~target:reference1 ~pattern:candidate2 ()))
    ~expect:(repeat (List.length unify_funcs) None)

let%test_unit "2 hole substitutions" =
  let expect = Some (String.Map.of_alist_exn [ ("1", Num 1); ("2", Num 2) ]) in
  [%test_result: substitutions option]
    (unify_naive ~debug:false ~target:reference1 ~pattern:candidate3 ())
    ~expect

let%test_unit "more complex hole substitution" =
  let expect =
    Some (String.Map.of_alist_exn [ ("1", Call (Name "*", [ Num 2; Num 3 ])) ])
  in
  [%test_result: substitutions option]
    (unify_naive ~debug:false ~target:reference2 ~pattern:candidate1 ())
    ~expect

let%test_unit "duplicate hole substitution" =
  let expect = Some (String.Map.of_alist_exn [ ("x", Num 2) ]) in
  [%test_result: substitutions option list]
    (List.map unify_funcs ~f:(fun unify ->
         unify ~debug:false ~target:reference4 ~pattern:candidate4 ()))
    ~expect:(repeat (List.length unify_funcs) expect)

let%test_unit "duplicate hole substitution with complex expr" =
  let expect =
    Some (String.Map.of_alist_exn [ ("x", Index (Name "a", Name "i")) ])
  in
  [%test_result: substitutions option list]
    (List.map unify_funcs ~f:(fun unify ->
         unify ~debug:false ~target:reference4' ~pattern:candidate4 ()))
    ~expect:(repeat (List.length unify_funcs) expect)

let%test_unit "duplicate hole substitution fail for unmatching exprs" =
  [%test_result: substitutions option list]
    (List.map unify_funcs ~f:(fun unify ->
         unify ~debug:false ~target:reference4'' ~pattern:candidate4 ()))
    ~expect:(repeat (List.length unify_funcs) None)

let%test_unit "unify sum" =
  let expect =
    Some
      (String.Map.of_alist_exn
         [ ("a", Name "c"); ("b", Name "i"); ("c", Name "x") ])
  in
  [%test_result: substitutions option list]
    (List.map unify_funcs ~f:(fun unify ->
         unify ~debug:false ~target:reference5 ~pattern:candidate5 ()))
    ~expect:(repeat (List.length unify_funcs) expect)

let%test_unit "self assign" =
  let expect = Some (String.Map.of_alist_exn [ ("a", Name "c") ]) in
  [%test_result: substitutions option list]
    (List.map unify_funcs ~f:(fun unify ->
         unify ~debug:false ~target:reference6 ~pattern:candidate6 ()))
    ~expect:(repeat (List.length unify_funcs) expect)

let%test_unit "index hole" =
  let expect = Some (String.Map.of_alist_exn [ ("a", Name "x") ]) in
  [%test_result: substitutions option list]
    (List.map unify_funcs ~f:(fun unify ->
         unify ~debug:false ~target:reference7 ~pattern:candidate7 ()))
    ~expect:(repeat (List.length unify_funcs) expect)

let%test_unit "unify mul" =
  let expect =
    Some
      (String.Map.of_alist_exn
         [ ("a", Name "z"); ("b", Name "x"); ("c", Name "i"); ("d", Name "y") ])
  in
  [%test_result: substitutions option list]
    (List.map unify_funcs ~f:(fun unify ->
         unify ~debug:false ~target:reference8 ~pattern:candidate8 ()))
    ~expect:(repeat (List.length unify_funcs) expect)

let%test_unit "unify mul commutative" =
  let expect =
    [ Some
        (String.Map.of_alist_exn
           [ ("a", Name "z")
           ; ("b", Name "x")
           ; ("c", Name "i")
           ; ("d", Name "y")
           ])
    ; None
    ]
  in
  [%test_result: substitutions option list]
    (List.map unify_funcs ~f:(fun unify ->
         unify ~target:reference8 ~pattern:candidate8' ()))
    ~expect

let%test_unit "unify where from canonical" =
  let expect =
    [ Some
        (String.Map.of_alist_exn
           [ ("cond", Name "x")
           ; ("cond2", Call (Name "broadcast", [ Num 0 ]))
           ; ("i", Name "i")
           ; ("out", Name "out")
           ; ("x", Name "x")
           ])
    ; None
    ]
  in
  [%test_result: substitutions option list]
    (List.map unify_funcs ~f:(fun unify ->
         unify ~debug:false ~target:reference9 ~pattern:candidate9 ()))
    ~expect

let%test_unit "unify where from np funcs" =
  let expect =
    [ Some
        (String.Map.of_alist_exn
           [ ("x", Name "x")
           ; ("z", Call (Name "broadcast", [ Num 0 ]))
           ; ("pos", Call (Name "broadcast", [ Num 1 ]))
           ; ("neg", Call (Name "broadcast", [ Num (-1) ]))
           ; ("arr", Name "x")
           ; ("where_i", Name "i")
           ; ("where_result", Name "out")
           ])
    ; None
    ]
  in
  [%test_result: substitutions option list]
    (List.map unify_funcs ~f:(fun unify ->
         unify
           ~debug:false
           ~target:reference9
           ~pattern:
             (Inline.inline_program candidate9'
             |> Partial_eval.partial_eval_program)
           ()))
    ~expect

let%test_unit "unify rolling sum via convolve" =
  let expect =
    [ Some
        (String.Map.of_alist_exn
           [ ("conv_i", Name "i")
           ; ("conv_j", Name "j")
           ; ("conv_result", Name "y")
           ; ("conv_sum", Name "s")
           ; ("w", Name "window_size")
           ; ("x", Name "x")
           ])
    ; None
    ]
  in
  [%test_result: substitutions option list]
    (List.map unify_funcs ~f:(fun unify ->
         unify ~debug:false ~target:reference10 ~pattern:candidate10 ()))
    ~expect
