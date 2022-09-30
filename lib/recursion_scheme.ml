open Core
open Lang

(* TODO: Assumes no mutual recursion? *)
let extract_list_foldr : datatype_env -> typ_env -> env -> string -> exp =
 fun sigma gamma env definition ->
  match Exp.decompose_abs (String.Map.find_exn env definition) with
  | params, EMatch (scrutinee, branches) ->
      let nil_param, nil_rhs =
        List.Assoc.find_exn ~equal:String.equal branches "Nil"
      in
      if String.Set.mem (Exp.free_variables nil_rhs) definition
      then failwith "recursive nil case"
      else (
        let cons_param, cons_rhs =
          List.Assoc.find_exn ~equal:String.equal branches "Cons"
        in
        let elem_type =
          match Type_system.ctor_typ sigma "Cons" with
          | Some (_, TProd (tau, _)) -> tau
          | _ -> failwith "non-prod Cons arg type"
        in
        let return_type =
          snd (Typ.decompose_arr (String.Map.find_exn gamma definition))
        in
        let new_nil_rhs = Exp.substitute (nil_param, EUnit) nil_rhs in
        (* TODO: Only supports exact same arguments except for recursive call on
                 final argument *)
        let new_cons_rhs =
          Exp.replace_subexp
            ~old_subexp:
              (Exp.build_app
                 (EVar definition)
                 (List.drop_last_exn (List.map ~f:(fun (x, _) -> EVar x) params)
                 @ [ ESnd (EVar cons_param) ]))
            ~new_subexp:(ESnd (EVar cons_param))
            cons_rhs
        in
        if String.Set.mem (Exp.free_variables new_cons_rhs) definition
        then failwith "could not parameterize recursion in cons case"
        else
          Exp.build_abs
            params
            (EApp
               ( ERScheme
                   (RListFoldr
                      ( new_nil_rhs
                      , EAbs
                          ( cons_param
                          , TProd (elem_type, return_type)
                          , new_cons_rhs ) ))
               , scrutinee )))
  | _ -> failwith "non-match under top-level abstractions"
