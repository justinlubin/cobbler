open Core
open Lang

let param_count : exp -> int = fun e -> List.length (Lang_util.params e)

let make_grammar : env -> id list * (int, id list, Int.comparator_witness) Map.t
  =
  Map.fold
    ~init:([], Map.empty (module Int))
    ~f:(fun ~key:lhs ~data:rhs (t_acc, nt_acc) ->
      let pc = param_count rhs in
      if Int.equal pc 0
      then (lhs :: t_acc, nt_acc)
      else (t_acc, Map.add_multi nt_acc ~key:pc ~data:lhs))

(* TODO: support more than size-3 nonterminals *)
let grow_nonterminals
    :  env -> (int, id list, Int.comparator_witness) Map.t -> exp list
    -> exp list
  =
 fun env nts space ->
  let open List.Let_syntax in
  let expansion1 =
    match Map.find nts 1 with
    | Some nts1 ->
        let%bind e1 = space in
        List.map ~f:(fun f -> EApp (EVar f, e1)) nts1
    | None -> []
  in
  let expansion2 =
    match Map.find nts 2 with
    | Some nts2 ->
        let%bind e1 = space in
        let%bind e2 = space in
        List.map ~f:(fun f -> EApp (EApp (EVar f, e1), e2)) nts2
    | None -> []
  in
  let expansion3 =
    match Map.find nts 3 with
    | Some nts3 ->
        let%bind e1 = space in
        let%bind e2 = space in
        let%bind e3 = space in
        List.map ~f:(fun f -> EApp (EApp (EApp (EVar f, e1), e2), e3)) nts3
    | None -> []
  in
  let additions =
    List.filter
      ~f:(fun e ->
        try
          ignore (Normalize.full env e);
          true
        with
        | _ -> false)
      (expansion1 @ expansion2 @ expansion3)
  in
  additions @ space

let synthesize : env -> exp -> exp option =
 fun env reference_program ->
  let normalized_reference_program = Normalize.full env reference_program in
  let env_terminals, env_nonterminals = make_grammar env in
  let reference_params = Lang_util.params reference_program in
  let all_terminals = env_terminals @ reference_params in
  Enumerative_search.bottom_up
    ~max_iterations:3
    ~initial_candidates:(List.map ~f:(fun x -> EVar x) all_terminals)
    ~grow:(grow_nonterminals env env_nonterminals)
    ~correct:(fun e ->
      printf "trying %s\n%!" (Lang_util.show_exp e);
      let result =
        Lang_util.alpha_equivalent
          (Lang_util.close_over reference_params (Normalize.full env e))
          normalized_reference_program
      in
      result)

type grammar = (typ, (id * typ list) list, Typ.comparator_witness) Map.t

let make_grammar' : typ_env -> env -> (id * typ) list -> grammar =
 fun gamma env free_vars ->
  Map.fold2
    gamma
    env
    ~init:
      (Map.of_alist_multi
         (module Typ)
         (List.map ~f:(fun (id, typ) -> (typ, (id, []))) free_vars))
    ~f:(fun ~key ~data acc ->
      match data with
      | `Right _ -> failwith "env contains key gamma does not"
      | `Left _ -> failwith "gamma contains key env does not"
      | `Both (typ, exp) ->
          let domain, codomain = Lang_util.decompose_arrow typ in
          Map.add_multi acc ~key:codomain ~data:(key, domain))

let make_app : id -> exp list -> exp =
 fun head args ->
  List.fold_left ~init:(EVar head) ~f:(fun acc x -> EApp (acc, x)) args

let expand : grammar -> exp -> exp list =
 fun grammar e ->
  let open List.Let_syntax in
  let rec expand' = function
    | EVar x -> [ EVar x ]
    | EApp (head, arg) ->
        let%bind head' = expand' head in
        let%bind arg' = expand' arg in
        [ EApp (head', arg') ]
    | EHole typ ->
        List.map
          ~f:(fun (x, typs) ->
            make_app x (List.map ~f:(fun typ -> EHole typ) typs))
          (Map.find_exn grammar typ)
    | EAbs _ | EMatch _ | ECtor _ | EInt _ ->
        failwith "expanding something other than var, app, or hole"
  in
  expand' e

let synthesize' : typ_env -> env -> (id * typ) list -> exp -> typ -> exp option =
 fun gamma env free_vars reference reference_typ ->
  let reference_params = List.map ~f:fst free_vars in
  let normalized_reference = Normalize.full env reference in
  let grammar = make_grammar' gamma env free_vars in
  Enumerative_search.top_down
    ~max_iterations:3
    ~start:(EHole reference_typ)
    ~expand:(expand grammar)
    ~correct:(fun e ->
      let result =
        Lang_util.alpha_equivalent
          (Lang_util.close_over reference_params (Normalize.full env e))
          normalized_reference
      in
      result)
