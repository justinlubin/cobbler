open Core
open Cbr_fp
open Lang
open Unification

let datatype_env1 = String.Map.of_alist_exn [ ("Gamma", [ ("B", TUnit) ]) ]

let%test_unit "unification 1" =
  let f = "(?? f (Gamma -> Gamma))" in
  let x = "(?? x Gamma)" in
  let e0 = Parse.exp (sprintf "(%s (%s %s))" f f x) in
  let e0' = Parse.exp (sprintf "(a (a b))") in
  let stdlib =
    String.Map.of_alist_exn
      [ ("a", TArr (TDatatype "Gamma", TDatatype "Gamma"))
      ; ("b", TDatatype "Gamma")
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
