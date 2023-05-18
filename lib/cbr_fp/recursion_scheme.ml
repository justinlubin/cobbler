open Core
open Lang

(* TODO: Assumes no mutual recursion? *)
let extract_list_foldr_exn : datatype_env -> typ_env -> env -> string -> exp =
 fun sigma gamma env name ->
  match Exp.decompose_abs (String.Map.find_exn env name) with
  | params, EMatch (scrutinee, branches) ->
      let nil_param, nil_rhs =
        match List.Assoc.find ~equal:String.equal branches "Nil" with
        | Some ([ nil_param ], nil_rhs) -> (nil_param, nil_rhs)
        | Some (_, _) -> failwith "malformatted Nil branch"
        | None -> failwith "missing Nil branch"
      in
      if String.Set.mem (Exp.free_variables nil_rhs) name
      then failwith "recursive Nil case"
      else (
        let cons_param, cons_rhs =
          match List.Assoc.find ~equal:String.equal branches "Cons" with
          | Some ([ cons_param ], cons_rhs) -> (cons_param, cons_rhs)
          | Some (_, _) -> failwith "malformatted Cons branch"
          | None -> failwith "missing Cons branch"
        in
        let elem_type =
          match Type_system.ctor_typ sigma "Cons" with
          | Some (_, [ TProd (tau, _) ]) -> tau
          | _ -> failwith "non-prod Cons arg type"
        in
        let return_type =
          snd (Typ.decompose_arr (String.Map.find_exn gamma name))
        in
        let new_nil_rhs = Exp.substitute (nil_param, EUnit) nil_rhs in
        (* TODO: Only supports exact same arguments except for recursive call on
                 final argument *)
        let new_cons_rhs =
          Exp.replace_subexp
            ~old_subexp:
              (Exp.build_app
                 (EVar name)
                 (List.drop_last_exn (List.map ~f:(fun (x, _) -> EVar x) params)
                 @ [ ESnd (EVar cons_param) ]))
            ~new_subexp:(ESnd (EVar cons_param))
            cons_rhs
        in
        if String.Set.mem (Exp.free_variables new_cons_rhs) name
        then failwith "could not parameterize recursion in Cons case"
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

let extract_list_foldr : datatype_env -> typ_env -> env -> string -> exp option =
 fun sigma gamma env name ->
  try Some (extract_list_foldr_exn sigma gamma env name) with
  | _ -> None
