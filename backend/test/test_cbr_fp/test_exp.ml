open Core
open Cbr_fp
open Lang

let%test_unit "substitute 1" =
  [%test_result: exp]
    ~equal:Exp.alpha_equivalent
    (Exp.substitute ("x", EVar "y") (EAbs ("x", EVar "x")))
    ~expect:(EAbs ("x", EVar "x"))

let%test_unit "substitute 2" =
  [%test_result: exp]
    ~equal:Exp.alpha_equivalent
    (Exp.substitute ("z", EVar "x") (EAbs ("x", EVar "z")))
    ~expect:(EAbs ("__var0", EVar "x"))

let%test_unit "substitute 3" =
  [%test_result: exp]
    ~equal:Exp.alpha_equivalent
    (Exp.substitute ("x", EVar "y") (EAbs ("x", EVar "x")))
    ~expect:(EAbs ("x", EVar "x"))

let%test_unit "alpha equivalent 1" =
  [%test_result: exp]
    ~equal:Exp.alpha_equivalent
    (EAbs ("x", EVar "x"))
    ~expect:(EAbs ("y", EVar "y"))

let%test_unit "normalizes cases 1" =
  [%test_eq: exp]
    (Exp.normalize
       String.Map.empty
       (EMatch
          ( ECtor ("Just", [ EVar "x" ])
          , [ ("Nothing", ([ "y" ], EVar "zero"))
            ; ("Just", ([ "z" ], ECtor ("Ok", [ EVar "z" ])))
            ] )))
    (ECtor ("Ok", [ EVar "x" ]))

let pattern_match ~reference ~sketch =
  Exp.pattern_match ~reference ~sketch
  |> Option.map
       ~f:(List.sort ~compare:(fun (s1, _) (s2, _) -> String.compare s1 s2))

let%test_unit "pattern match 1" =
  [%test_eq: (string * exp) list option]
    (pattern_match
       ~reference:
         (EMatch
            ( ECtor ("Just", [ EVar "x" ])
            , [ ("Nothing", ([ "y" ], EVar "zero"))
              ; ("Just", ([ "z" ], ECtor ("Ok", [ EVar "z" ])))
              ] ))
       ~sketch:
         (EMatch
            ( ECtor ("Just", [ EVar "x" ])
            , [ ("Nothing", ([ "y" ], EVar "zero"))
              ; ("Just", ([ "z" ], ECtor ("Ok", [ EVar "z" ])))
              ] )))
    (Some [])

let%test_unit "pattern match 2" =
  [%test_eq: (string * exp) list option]
    (pattern_match
       ~reference:
         (EMatch
            ( ECtor ("Just", [ EVar "z" ])
            , [ ("Nothing", ([ "y" ], EVar "zero"))
              ; ("Just", ([ "q" ], ECtor ("Ok", [ EVar "q" ])))
              ] ))
       ~sketch:
         (EMatch
            ( ECtor ("Just", [ EVar "x" ])
            , [ ("Nothing", ([ "y" ], EVar "zero"))
              ; ("Just", ([ "z" ], ECtor ("Ok", [ EVar "z" ])))
              ] )))
    (Some [])

let%test_unit "pattern match 3" =
  [%test_eq: (string * exp) list option]
    (pattern_match
       ~reference:
         (EMatch
            ( ECtor ("Just", [ EVar "x" ])
            , [ ("Nothing", ([ "y" ], EVar "zero"))
              ; ("Just", ([ "z" ], ECtor ("Ok", [ EVar "z" ])))
              ] ))
       ~sketch:
         (EMatch
            ( EHole ("h1", TBase BTInt)
            , [ ("Nothing", ([ "y" ], EVar "zero"))
              ; ("Just", ([ "z" ], ECtor ("Ok", [ EVar "z" ])))
              ] )))
    (Some [ ("h1", ECtor ("Just", [ EVar "x" ])) ])

let%test_unit "pattern match 4" =
  [%test_eq: (string * exp) list option]
    (pattern_match
       ~reference:
         (EMatch
            ( ECtor ("Just", [ EVar "x" ])
            , [ ("Nothing", ([ "y" ], EVar "zero"))
              ; ("Just", ([ "z" ], ECtor ("Ok", [ EVar "z" ])))
              ] ))
       ~sketch:
         (EMatch
            ( EHole ("h1", TBase BTInt)
            , [ ("Nothing", ([ "y" ], EHole ("h2", TBase BTInt)))
              ; ("Just", ([ "z" ], ECtor ("Ok", [ EVar "z" ])))
              ] )))
    (Some [ ("h1", ECtor ("Just", [ EVar "x" ])); ("h2", EVar "zero") ])

let%test_unit "pattern match 5" =
  [%test_eq: (string * exp) list option]
    (pattern_match
       ~reference:
         (EMatch
            ( ECtor ("Just", [ EVar "x" ])
            , [ ("Nothing", ([ "y" ], EVar "zero"))
              ; ("Just", ([ "z" ], ECtor ("Ok", [ EVar "z" ])))
              ] ))
       ~sketch:
         (EMatch
            ( EHole ("h1", TBase BTInt)
            , [ ("Nothing", ([ "y" ], EHole ("h2", TBase BTInt)))
              ; ("Just", ([ "z" ], EHole ("h2", TBase BTInt)))
              ] )))
    None

let%test_unit "pattern match 6" =
  [%test_eq: (string * exp) list option]
    (pattern_match
       ~reference:
         (EMatch
            ( ECtor ("Just", [ EVar "x" ])
            , [ ("Nothing", ([ "y" ], EVar "zero"))
              ; ("Just", ([ "z" ], EVar "zero"))
              ] ))
       ~sketch:
         (EMatch
            ( EHole ("h1", TBase BTInt)
            , [ ("Nothing", ([ "y" ], EHole ("h2", TBase BTInt)))
              ; ("Just", ([ "z" ], EHole ("h2", TBase BTInt)))
              ] )))
    (Some [ ("h1", ECtor ("Just", [ EVar "x" ])); ("h2", EVar "zero") ])

let%test_unit "pattern match 7" =
  [%test_eq: (string * exp) list option]
    (pattern_match
       ~reference:
         (EAbs
            ("f", ECtor ("Just", [ EApp (EVar "f", EApp (EVar "f", EVar "x")) ])))
       ~sketch:
         (EAbs
            ( "f"
            , ECtor
                ( "Just"
                , [ EApp
                      ( EHole ("h1", TBase BTInt)
                      , EApp (EHole ("h1", TBase BTInt), EVar "x") )
                  ] ) )))
    (Some [ ("h1", EVar "f") ])
