open Core
open Lang

let params : exp -> id list =
 fun e ->
  let rec param_count' acc = function
    | EAbs (param, body) -> param_count' (param :: acc) body
    | _ -> List.rev acc
  in
  param_count' [] e

let param_count : exp -> int = fun e -> List.length (params e)

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
    : (int, id list, Int.comparator_witness) Map.t -> exp list -> exp list
  =
 fun nts plist ->
  let open List.Let_syntax in
  let expansion1 =
    match Map.find nts 1 with
    | Some nts1 ->
        let%bind e1 = plist in
        List.map ~f:(fun f -> EApp (EVar f, e1)) nts1
    | None -> []
  in
  let expansion2 =
    match Map.find nts 2 with
    | Some nts2 ->
        let%bind e1 = plist in
        let%bind e2 = plist in
        List.map ~f:(fun f -> EApp (EApp (EVar f, e1), e2)) nts2
    | None -> []
  in
  let expansion3 =
    match Map.find nts 3 with
    | Some nts3 ->
        let%bind e1 = plist in
        let%bind e2 = plist in
        let%bind e3 = plist in
        List.map ~f:(fun f -> EApp (EApp (EApp (EVar f, e1), e2), e3)) nts3
    | None -> []
  in
  plist @ expansion1 @ expansion2 @ expansion3

let close_over : id list -> exp -> exp =
 fun ids e -> List.fold_right ~init:e ~f:(fun id acc -> EAbs (id, acc)) ids

let synthesize : env -> exp -> exp option =
 fun env reference_program ->
  let normalized_reference_program = Normalize.full env reference_program in
  let env_terminals, env_nonterminals = make_grammar env in
  let reference_params = params reference_program in
  let all_terminals = env_terminals @ reference_params in
  Enumerate.search
    ~max_iterations:3
    ~terminals:(List.map ~f:(fun x -> EVar x) all_terminals)
    ~grow:(grow_nonterminals env_nonterminals)
    ~prune:
      (List.filter ~f:(fun e ->
           try
             ignore (Normalize.full env e);
             true
           with
           | _ -> false))
    ~is_correct:(fun e ->
      let result =
        Lang_util.alpha_equivalent
          (close_over reference_params (Normalize.full env e))
          normalized_reference_program
      in
      result)
