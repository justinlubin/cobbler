open Core
open Lang

let inline : library -> exp -> exp =
 fun lib e ->
  let rec inline' shadows = function
    | EVar x -> if Set.mem shadows x then EVar x else Map.find_exn lib x
    | EApp (e1, e2) -> EApp (inline' shadows e1, inline' shadows e2)
    | EAbs (x, e1) -> EAbs (x, inline' (Set.add shadows x) e1)
  in
  inline' (Set.empty (module String)) e

let partially_evaluate_cases : exp -> exp = fun e -> failwith "TODO"
let pull_out_cases : exp -> exp = fun e -> failwith "TODO"
let full : library -> exp -> exp = fun lib e -> failwith "TODO"
