open Core
open Lang

let replace_subexp : old_subexp:exp -> new_subexp:exp -> exp -> exp =
 fun ~old_subexp ~new_subexp e ->
  let rec recurse = function
    | EVar x -> EVar x
    | EApp (head, arg) -> EApp (check_and_replace head, check_and_replace arg)
    | EAbs (param, tau, body) -> EAbs (param, tau, check_and_replace body)
    | EMatch (scrutinee, branches) ->
        EMatch
          ( check_and_replace scrutinee
          , Exp.map_branches ~f:check_and_replace branches )
    | ECtor (ctor_name, arg) -> ECtor (ctor_name, check_and_replace arg)
    | EPair (e1, e2) -> EPair (check_and_replace e1, check_and_replace e2)
    | EFst arg -> EFst (check_and_replace arg)
    | ESnd arg -> ESnd (check_and_replace arg)
    | EUnit -> EUnit
    | EInt n -> EInt n
    | EHole (name, typ) -> EHole (name, typ)
  and check_and_replace e =
    if [%eq: exp] e old_subexp then new_subexp else recurse e
  in
  check_and_replace e

(* TODO: Assumes no mutual recursion? *)
let extract_list_foldr : datatype_env -> typ_env -> env -> string -> exp =
 fun sigma gamma env definition ->
  match Exp.decompose_abs (String.Map.find_exn env definition) with
  | params, EMatch (scrutinee, branches) ->
      let nil_arg, nil_rhs =
        List.Assoc.find_exn ~equal:String.equal branches "Nil"
      in
      if String.Set.mem (Exp.free_variables nil_rhs) definition
      then failwith "recursive nil case"
      else (
        let cons_arg, cons_rhs =
          List.Assoc.find_exn ~equal:String.equal branches "Cons"
        in
        let fold_var = "__list_foldr" in
        let parameterized_rec_var = Util.gensym "rec" in
        (* TODO: Only supports exact same arguments except for recursive call on
                 final argument *)
        let new_cons_rhs =
          replace_subexp
            ~old_subexp:
              (Exp.build_app
                 (EVar definition)
                 (List.drop_last_exn (List.map ~f:(fun (x, _) -> EVar x) params)
                 @ [ ESnd (EVar cons_arg) ]))
            ~new_subexp:(EVar parameterized_rec_var)
            cons_rhs
        in
        if String.Set.mem (Exp.free_variables new_cons_rhs) definition
        then failwith "could not parameterize recursion in cons case"
        else
          Exp.build_abs
            params
            (Exp.build_app
               (EVar fold_var)
               [ scrutinee
               ; Exp.build_abs
                   [ ( nil_arg
                     , snd (Option.value_exn (Type_system.ctor_typ sigma "Nil"))
                     )
                   ]
                   nil_rhs
               ; Exp.build_abs
                   [ ( cons_arg
                     , snd
                         (Option.value_exn (Type_system.ctor_typ sigma "Cons"))
                     )
                   ; ( parameterized_rec_var
                     , snd
                         (Typ.decompose_arr
                            (String.Map.find_exn gamma definition)) )
                   ]
                   new_cons_rhs
               ]))
  | _ -> failwith "non-match under top-level abstractions"
