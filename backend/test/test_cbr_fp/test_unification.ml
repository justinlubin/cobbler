open Core
open Cbr_fp
open Lang
open Unification

let datatype_env1 = String.Map.of_alist_exn [ ("Gamma", ([], [ ("B", []) ])) ]

let%test_unit "unification 1" =
  let f = "(?? f ((Gamma) -> (Gamma)))" in
  let x = "(?? x (Gamma))" in
  let e0 = Parse.exp (sprintf "(%s (%s %s))" f f x) in
  let e0' = Parse.exp (sprintf "(a (a b))") in
  let stdlib =
    String.Map.of_alist_exn
      [ ("a", ([], TArr (TDatatype ("Gamma", []), TDatatype ("Gamma", []))))
      ; ("b", ([], TDatatype ("Gamma", [])))
      ]
  in
  match
    Unification.unify
      100
      (Unification_adapter.to_unification_term datatype_env1 stdlib e0)
      (Unification_adapter.to_unification_term datatype_env1 stdlib e0')
  with
  | Solved subs -> ()
  | Impossible -> failwith "shouldn't be impossible"
  | OutOfFuel -> failwith "shouldn't run out of fuel"

let%test_unit "unification 2" =
  let bt = Elementary (TVar "base") in
  let catat = Arrow (Arrow (bt, bt), Arrow (bt, Arrow (bt, bt))) in
  let u1 =
    Application
      ( Application
          ( Application
              ( Atom (Constant ("__cata#147$Maybe", catat))
              , Abstraction
                  ( "__cata_z#136"
                  , bt
                  , Application
                      ( Atom (Variable ("__ua_hole^__hole#76", Arrow (bt, bt)))
                      , Atom (Variable ("__cata_z#136", bt)) ) ) )
          , Atom (Variable ("__ua_hole^__hole#56", bt)) )
      , Atom (Variable ("__ua_hole^__hole#77", bt)) )
  in
  let u2 =
    Application
      ( Application
          ( Application
              ( Atom (Constant ("__cata#38$Maybe", catat))
              , Abstraction
                  ( "__var#28"
                  , Elementary (TVar "____typevar#49")
                  , Application
                      ( Atom
                          (Constant
                             ( "__var#26"
                             , Arrow
                                 ( Elementary (TVar "____typevar#25")
                                 , Elementary (TDatatype ("Bool", [])) ) ))
                      , Atom
                          (Variable
                             ( "__var#28"
                             , bt (*Elementary (TVar "____typevar#49"))*) )) )
                  ) )
          , Atom
              (Constant ("__ctor#45$False", Elementary (TDatatype ("Bool", []))))
          )
      , Atom
          (Constant
             ( "__var#27"
             , Elementary (TDatatype ("Maybe", [ TVar "____typevar#25" ])) )) )
  in
  print_endline (Unification.show_term u1);
  print_endline "";
  print_endline (Unification.show_term u2);
  match Unification.unify 100 u1 u2 with
  | Solved subs -> ()
  | Impossible -> failwith "shouldn't be impossible"
  | OutOfFuel -> failwith "shouldn't run out of fuel"

let%test_unit "unification 3" =
  let bt = Elementary (TVar "base") in
  let bt2 = Elementary (TVar "base2") in
  let u1 =
    Abstraction
      ( "x"
      , bt
      , Application
          (Atom (Variable ("f", Arrow (bt, bt))), Atom (Variable ("x", bt))) )
  in
  let u2 =
    Abstraction
      ( "y"
      , bt
      , Application
          (Atom (Constant ("f", Arrow (bt, bt))), Atom (Variable ("y", bt2))) )
  in
  print_endline (Unification.show_term u1);
  print_endline "";
  print_endline (Unification.show_term u2);
  match Unification.unify 100 u1 u2 with
  | Solved subs -> ()
  | Impossible -> failwith "shouldn't be impossible"
  | OutOfFuel -> failwith "shouldn't run out of fuel"
