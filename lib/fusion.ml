open Core
open Lang

let rec pull_out_cases : exp -> exp = function
  | EVar x -> EVar x
  | EApp (head, arg) -> EApp (pull_out_cases head, pull_out_cases arg)
  | EAbs (param, tau, body) -> EAbs (param, tau, pull_out_cases body)
  | EMatch (outer_scrutinee, outer_branches) ->
      let outer_branches' = Exp.map_branches ~f:pull_out_cases outer_branches in
      (match pull_out_cases outer_scrutinee with
      | EMatch (inner_scrutinee, inner_branches) ->
          EMatch
            ( inner_scrutinee
            , Exp.map_branches
                ~f:(fun rhs -> EMatch (rhs, outer_branches'))
                inner_branches )
      | outer_scrutinee' -> EMatch (outer_scrutinee', outer_branches'))
  | ECtor (ctor_name, arg) -> ECtor (ctor_name, pull_out_cases arg)
  | EPair (e1, e2) -> EPair (pull_out_cases e1, pull_out_cases e2)
  | EFst arg -> EFst (pull_out_cases arg)
  | ESnd arg -> ESnd (pull_out_cases arg)
  | EUnit -> EUnit
  | EInt n -> EInt n
  | EHole (name, typ) -> EHole (name, typ)

(* let contains : string -> exp -> bool = fun s e -> failwith "TODO"
let transform : exp -> exp -> exp = fun e1 e2 -> failwith "TODO" *)

(* Assumes no mutual recursion? *)
let extract_list_fold : recursive_name:string -> exp -> exp =
 fun ~recursive_name e -> failwith "TODO"
(* fun ~recursive_name -> function
  | EMatch (scrutinee, branches) ->
      let _, nil_rhs = List.Assoc.find_exn ~equal:String.equal branches "Nil" in
      if contains recursive_name nil_rhs
      then failwith "recursive nil case"
      else (
        let _, cons_rhs =
          List.Assoc.find_exn ~equal:String.equal branches "Cons"
        in
        let rec_var = Util.gensym "rec" in
        (* Exp.build_app (EVar "__list_fold") [ scrutinee; nil_rhs ; build_abs [();()] (transform cons_rhs)] *)
        failwith "TODO: need pairs")
  | _ -> failwith "TODO" *)
