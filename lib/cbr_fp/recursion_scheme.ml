open Core
open Lang

(* TODO: Assumes no mutual recursion? *)
let extract_list_foldr_exn : datatype_env -> typ_env -> env -> string -> exp =
 fun sigma gamma env name ->
  match Exp.decompose_abs (String.Map.find_exn env name) with
  | params, EMatch (scrutinee, branches) ->
      let nil_rhs =
        match List.Assoc.find ~equal:String.equal branches "Nil" with
        | Some ([], nil_rhs) -> nil_rhs
        | Some (_, _) -> failwith "malformatted Nil branch"
        | None -> failwith "missing Nil branch"
      in
      if String.Set.mem (Exp.free_variables nil_rhs) name
      then failwith "recursive Nil case"
      else (
        let cons_hd_param, cons_tl_param, cons_rhs =
          match List.Assoc.find ~equal:String.equal branches "Cons" with
          | Some ([ hd; tl ], cons_rhs) -> (hd, tl, cons_rhs)
          | Some (_, _) -> failwith "malformatted Cons branch"
          | None -> failwith "missing Cons branch"
        in
        let elem_type =
          match Type_system.ctor_typ sigma "Cons" with
          | Some (_, [ tau; _ ]) -> tau
          | _ -> failwith "incorrect Cons arg type"
        in
        let return_type =
          snd (Typ.decompose_arr (String.Map.find_exn gamma name))
        in
        (* TODO: Only supports exact same arguments except for recursive call on
                 final argument *)
        let new_cons_rhs =
          Exp.replace_subexp
            ~old_subexp:
              (Exp.build_app
                 (EVar name)
                 (List.drop_last_exn (List.map ~f:(fun (x, _) -> EVar x) params)
                 @ [ EVar cons_tl_param ]))
            ~new_subexp:(EVar cons_tl_param)
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
                      ( nil_rhs
                      , EAbs
                          ( cons_hd_param
                          , elem_type
                          , EAbs (cons_tl_param, return_type, new_cons_rhs) ) ))
               , scrutinee )))
  | _ -> failwith "non-match under top-level abstractions"

let extract_list_foldr : datatype_env -> typ_env -> env -> string -> exp option =
 fun sigma gamma env name ->
  try Some (extract_list_foldr_exn sigma gamma env name) with
  | _ -> None
