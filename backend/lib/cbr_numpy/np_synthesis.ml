open Lang
open Env
open Core

let make_grammar_entry
    :  String.Set.t -> required:String.Set.t -> string -> hole_type list
    -> expr option
  =
 fun fvs ~required lhs rhs ->
  if Set.is_subset required ~of_:fvs
  then
    Some
      (Call (Name lhs, List.map rhs ~f:(fun t -> Hole (t, Util.gensym "hole"))))
  else None

let make_grammar_entries
    : String.Set.t -> (string * string list * hole_type list) list -> expr list
  =
 fun fvs entries ->
  entries
  |> List.filter_map ~f:(fun (lhs, req, rhs) ->
         make_grammar_entry fvs ~required:(String.Set.of_list req) lhs rhs)

let make_grammar : String.Set.t -> hole_type -> expr list =
 fun fvs tau ->
  match tau with
  | Constant -> []
  | List ->
      make_grammar_entries
        fvs
        [ ("np.tolist", [], [ Array; Constant ])
        ; ("np.filter", [], [ Array; Constant ])
        ]
  | String -> []
  | Number ->
      make_grammar_entries
        fvs
        [ ("np.sum", [ "+" ], [ Array ]); ("np.prod", [ "*" ], [ Array ]) ]
  | Array ->
      make_grammar_entries
        fvs
        [ ("np.multiply", [ "*" ], [ Array; Array ])
        ; ("np.divide", [ "/" ], [ Array; Array ])
        ; ("np.add", [ "+" ], [ Array; Array ])
        ; ("np.subtract", [ "-" ], [ Array; Array ])
        ; ("np.power", [ "**" ], [ Array; Array ])
        ; ("np.equal", [ "==" ], [ Array; Array ])
        ; ("np.not_equal", [ "!=" ], [ Array; Array ])
        ; ("np.full", [], [ Number; Constant ])
        ; ("np.greater", [ ">" ], [ Array; Array ])
        ; ("np.greater_equal", [ ">=" ], [ Array; Array ])
        ; ("np.less", [ "<" ], [ Array; Array ])
        ; ("np.less_equal", [ "<=" ], [ Array; Array ])
        ; ("np.where", [], [ Array; Array; Array ])
        ; ("np.roll", [], [ Array; Number ])
        ; ("np.convolve_valid", [], [ Array; Array ])
        ; ( "np.random.randint_size"
          , [ "np.random.randint" ]
          , [ Number; Number; Number ] )
        ; ("range", [], [ Number ])
        ; ("np.copy", [], [ Array; Constant ])
        ]

let expand : String.Set.t -> int -> expr -> expr list =
 fun fvs _ e ->
  let rec expand' = function
    | Num n -> [ Num n ]
    | Str s -> [ Str s ]
    | Name id -> [ Name id ]
    | Hole (tau, _) -> make_grammar fvs tau
    | Index (head, index) ->
        let expanded_head = expand' head in
        let expanded_index = expand' index in
        List.map ~f:(fun h : expr -> Index (h, index)) expanded_head
        @ List.map ~f:(fun i : expr -> Index (head, i)) expanded_index
    | Call (fn, args) ->
        let expanded_fn = expand' fn in
        let expanded_args =
          match args with
          | [ e1 ] -> List.map ~f:(fun e' -> [ e' ]) (expand' e1)
          | [ e1; e2 ] ->
              let expanded_e1 = expand' e1 in
              let expanded_e2 = expand' e2 in
              List.map ~f:(fun e' -> [ e'; e2 ]) expanded_e1
              @ List.map ~f:(fun e' -> [ e1; e' ]) expanded_e2
          | [ e1; e2; e3 ] ->
              let expanded_e1 = expand' e1 in
              let expanded_e2 = expand' e2 in
              let expanded_e3 = expand' e3 in
              List.map ~f:(fun e' -> [ e'; e2; e3 ]) expanded_e1
              @ List.map ~f:(fun e' -> [ e1; e'; e3 ]) expanded_e2
              @ List.map ~f:(fun e' -> [ e1; e2; e' ]) expanded_e3
          (* only handles unary, 2-ary, and 3-ary function calls *)
          | _ -> []
        in
        List.map ~f:(fun fn' -> Call (fn', args)) expanded_fn
        @ List.map ~f:(fun args' -> Call (fn, args')) expanded_args
  in
  expand' e

(* Fails if substitution does not respect hole type *)

exception IncorrectSubstitutionType of hole_type * expr

let rec is_constant : expr -> bool = function
  | Num _ | Str _ | Name _ | Hole (_, _) -> true
  | Index (e1, e2) -> is_constant e1 && is_constant e2
  | Call (Name "len", [ arg ])
  | Call (Name "__memberAccess", [ _; arg ])
  | Call (Call (Name "np.vectorize", [ Name "__memberAccess"; _ ]), [ _; arg ])
  | Call (Name "np.vectorize", [ arg; _ ]) -> is_constant arg
  | Call
      ( Name
          ( "+"
          | "*"
          | "-"
          | "/"
          | "**"
          | "=="
          | "!="
          | ">"
          | ">="
          | "<"
          | "<="
          | "%" )
      , args ) -> List.for_all ~f:is_constant args
  | Call (_, _) -> false

let binding_ok : hole_type -> expr -> bool =
 fun tau e ->
  match tau with
  | Constant -> is_constant e
  | _ -> true

let substitute_expr : expr -> substitutions -> expr option =
 fun e subs ->
  let rec substitute = function
    | Num n -> Num n
    | Str s -> Str s
    | Name id -> Name id
    | Index (hd, index) -> Index (substitute hd, substitute index)
    | Call (fn, args) -> Call (substitute fn, List.map ~f:substitute args)
    | Hole (tau, h) as e' ->
        (match Map.find subs h with
        | Some binding ->
            if binding_ok tau binding
            then binding
            else raise (IncorrectSubstitutionType (tau, binding))
        | None -> e')
  in
  try Some (substitute e) with
  | IncorrectSubstitutionType (tau, binding) -> None

let canonicalize : program -> program =
  Util.Timing_breakdown.record1 Util.Timing_breakdown.Canonicalization
  @@ fun p -> p |> Inline.inline_program |> Partial_eval.partial_eval_program

let rec fix_filter : expr -> expr =
  let fix_filter_pred array pred =
    let rec fix_filter_pred' e =
      match e with
      | Index (e1, Name i) when [%eq: expr] e1 array -> (array, [ i ])
      | Index (e1, e2) ->
          let e1', ce1 = fix_filter_pred' e1 in
          let e2', ce2 = fix_filter_pred' e2 in
          (Index (e1', e2'), ce1 @ ce2)
      | Call
          ( (Name
               ( "+"
               | "*"
               | "-"
               | "/"
               | "**"
               | "=="
               | "!="
               | ">"
               | ">="
               | "<"
               | "<="
               | "%" ) as head)
          , args ) ->
          let a, ca = List.map ~f:fix_filter_pred' args |> List.unzip in
          (Call (head, a), List.concat ca)
      | Call (head, args) ->
          let h, ch = fix_filter_pred' head in
          let a, ca = List.map ~f:fix_filter_pred' args |> List.unzip in
          (Call (Call (Name "np.vectorize", [ h ]), a), ch @ List.concat ca)
      | Num _ | Str _ | Name _ | Hole (_, _) -> (e, [])
    in
    let pred', cs = fix_filter_pred' pred in
    if List.is_empty cs
    then pred'
    else (
      match List.all_equal ~equal:String.equal cs with
      | Some _ -> pred'
      | None -> pred)
  in
  fun e ->
    match e with
    | Call (Name "np.filter", [ array; pred ]) ->
        let fixed_array = fix_filter array in
        Call (Name "np.filter", [ fixed_array; fix_filter_pred array pred ])
    | Call (head, args) -> Call (fix_filter head, List.map ~f:fix_filter args)
    | Index (e1, e2) -> Index (fix_filter e1, fix_filter e2)
    | Num _ | Str _ | Name _ | Hole (_, _) -> e

let eq_len : expr -> expr -> bool =
 fun e1 e2 ->
  [%eq: expr]
    (Partial_eval.partial_eval_expr (Call (Name "len", [ e1 ])))
    (Partial_eval.partial_eval_expr (Call (Name "len", [ e2 ])))

let rec simplify : expr -> expr =
 fun e ->
  match e with
  | Index (e1, e2) -> Index (simplify e1, simplify e2)
  | Call (fn, args) ->
      let fn = simplify fn in
      let args = List.map ~f:simplify args in
      (match (fn, args) with
      (* tolist *)
      | Name "np.tolist", [ arg; amount ] ->
          simplify
            (Call
               (Name "sliceUntil", [ Call (Name "np.tolist", [ arg ]); amount ]))
      (* Add/Product identities *)
      | ( Name "np.add"
        , [ ( Call (Name "np.zeros", _)
            | Call (Name "np.full", [ _; Num 0 ])
            | Num 0 )
          ; arg
          ] )
      | ( Name "np.add"
        , [ arg
          ; ( Call (Name "np.zeros", _)
            | Call (Name "np.full", [ _; Num 0 ])
            | Num 0 )
          ] )
      | ( Name "np.product"
        , [ (Call (Name "np.full", [ _; Num 1 ]) | Num 1); arg ] )
      | ( Name "np.product"
        , [ arg; (Call (Name "np.full", [ _; Num 1 ]) | Num 1) ] ) ->
          simplify arg
      (* Copy *)
      | Name "np.copy", [ arg; amount ] ->
          simplify
            (Call (Name "sliceUntil", [ Call (Name "np.copy", [ arg ]); amount ]))
      | Name "np.copy", [ (Call (Call (Name "np.vectorize", _), _) as inner) ]
        -> simplify inner
      (* sliceToEnd *)
      | Name "len", [ Call (Name "sliceToEnd", [ a; x ]) ] ->
          simplify (Call (Name "-", [ Call (Name "len", [ a ]); x ]))
      | ( Name "sliceToEnd"
        , [ a; Call (Name "-", [ Call (Name "len", [ a' ]); x ]) ] )
        when eq_len a a' ->
          simplify
            (Call (Name "sliceToEnd", [ a; Call (Name "negate", [ x ]) ]))
      | Name "sliceToEnd", [ a; Num 0 ] -> a
      | Name "sliceToEnd", [ Call (Name "broadcast", [ n ]); _ ] ->
          Call (Name "broadcast", [ n ])
      | ( Name "sliceToEnd"
        , [ Call
              ( (Name
                   ( "np.multiply"
                   | "np.divide"
                   | "np.add"
                   | "np.subtract"
                   | "np.power"
                   | "np.equal"
                   | "np.greater"
                   | "np.greater_equal"
                   | "np.less"
                   | "np.less_equal"
                   | "np.where"
                   | "np.tolist"
                   | "np.copy" ) as inner_f)
              , inner_args )
          ; n
          ] ) ->
          simplify
            (Call
               ( inner_f
               , List.map inner_args ~f:(fun a ->
                     Call (Name "sliceToEnd", [ a; n ])) ))
      (* sliceUntil *)
      | Name "len", [ Call (Name "sliceUntil", [ _; x ]) ] -> simplify x
      | ( Name "sliceUntil"
        , [ Call (Name "np.random.randint_size", [ low; high; _ ]); x ] ) ->
          simplify (Call (Name "np.random.randint_size", [ low; high; x ]))
      | Name "sliceUntil", [ Call (Name "np.full", [ _; v ]); x ] ->
          simplify (Call (Name "np.full", [ x; v ]))
      | Name "sliceUntil", [ Call (Name "range", [ _ ]); x ] ->
          simplify (Call (Name "range", [ x ]))
      | ( Name "sliceUntil"
        , [ a; Call (Name "-", [ Call (Name "len", [ a' ]); x ]) ] )
        when eq_len a a' ->
          simplify
            (Call (Name "sliceUntil", [ a; Call (Name "negate", [ x ]) ]))
      | Name "sliceUntil", [ a; Call (Name "len", [ a' ]) ] when eq_len a a' ->
          a
      | Name "sliceUntil", [ Call (Name "broadcast", [ n ]); _ ] ->
          Call (Name "broadcast", [ n ])
      | ( Name "sliceUntil"
        , [ Call
              ( (Name
                   ( "np.multiply"
                   | "np.divide"
                   | "np.add"
                   | "np.subtract"
                   | "np.power"
                   | "np.equal"
                   | "np.greater"
                   | "np.greater_equal"
                   | "np.less"
                   | "np.less_equal"
                   | "np.where"
                   | "np.tolist"
                   | "np.copy" ) as inner_f)
              , inner_args )
          ; n
          ] ) ->
          simplify
            (Call
               ( inner_f
               , List.map inner_args ~f:(fun a ->
                     Call (Name "sliceUntil", [ a; n ])) ))
      (* Propagation *)
      | ( Name "len"
        , [ Call
              ( Name
                  ( "np.multiply"
                  | "np.divide"
                  | "np.add"
                  | "np.subtract"
                  | "np.power"
                  | "np.equal"
                  | "np.greater"
                  | "np.greater_equal"
                  | "np.less"
                  | "np.less_equal"
                  | "np.where" )
              , hd :: _ )
          ] ) -> simplify (Call (Name "len", [ hd ]))
      | Name "len", [ Call (Name ("range" | "broadcast" | "np.zeros"), [ hd ]) ]
        -> simplify hd
      | Name "len", [ Call (Name "np.full", [ hd; _ ]) ] -> simplify hd
      | Name "len", [ Call (Name "np.random.randint_size", [ _; _; s ]) ] ->
          simplify s
      (* Default *)
      | _ -> Call (fn, args))
  | Num _ | Str _ | Name _ | Hole (_, _) -> e

let rec cap_second_arguments : expr -> expr =
 fun e ->
  match e with
  | Index (e1, e2) -> Index (cap_second_arguments e1, cap_second_arguments e2)
  | Call (fn, args) ->
      let fn = cap_second_arguments fn in
      let args = List.map ~f:cap_second_arguments args in
      (match (fn, args) with
      | ( ( Name "np.multiply"
          | Name "np.divide"
          | Name "np.add"
          | Name "np.subtract"
          | Name "np.power"
          | Name "np.equal"
          | Name "np.greater"
          | Name "np.greater_equal"
          | Name "np.less"
          | Name "np.less_equal" )
        , [ arg1; arg2 ] ) ->
          Call
            ( fn
            , [ arg1
              ; Call (Name "sliceUntil", [ arg2; Call (Name "len", [ arg1 ]) ])
              ] )
      | _ -> Call (fn, args))
  | Num _ | Str _ | Name _ | Hole (_, _) -> e

let clean : expr -> expr = fun e -> simplify (cap_second_arguments (simplify e))

let rec base_pat_name : pat -> id option =
 fun p ->
  match p with
  | PName x -> Some x
  | PIndex (p', _) -> base_pat_name p'
  | PHole (_, _) -> None

let rec referenced_vars_expr : expr -> String.Set.t =
 fun e ->
  match e with
  | Name x -> String.Set.singleton x
  | Index (e1, e2) ->
      Set.union (referenced_vars_expr e1) (referenced_vars_expr e2)
  | Call (head, args) ->
      String.Set.union_list
        (referenced_vars_expr head :: List.map ~f:referenced_vars_expr args)
  | Str _ | Num _ | Hole (_, _) -> String.Set.empty

and referenced_vars_stmt : stmt -> String.Set.t =
 fun s ->
  match s with
  | For (_, e, b) ->
      Set.union (referenced_vars_expr e) (referenced_vars_block b)
  | If (cond, then_branch, else_branch) ->
      referenced_vars_expr cond
      |> Set.union (referenced_vars_block then_branch)
      |> Set.union (referenced_vars_block else_branch)
  | Assign (_, e) | Return e -> referenced_vars_expr e

and referenced_vars_block : block -> String.Set.t =
 fun b -> b |> List.map ~f:referenced_vars_stmt |> String.Set.union_list

and referenced_vars : program -> String.Set.t =
 fun (_, b) -> referenced_vars_block b

let rec loop_vars_stmt : stmt -> String.Set.t =
 fun s ->
  match s with
  | For (p, _, b) ->
      let vs = loop_vars_block b in
      (match base_pat_name p with
      | Some x -> Set.add vs x
      | None -> vs)
  | If (_, then_branch, else_branch) ->
      Set.union (loop_vars_block then_branch) (loop_vars_block else_branch)
  | Assign (_, _) | Return _ -> String.Set.empty

and loop_vars_block : block -> String.Set.t =
 fun b -> b |> List.map ~f:loop_vars_stmt |> String.Set.union_list

let loop_vars : program -> String.Set.t = fun (_, b) -> loop_vars_block b

exception EarlyCutoff of string

let infer : program -> string * hole_type list =
 fun (_, b) ->
  match List.last_exn b with
  | Return (Name var) ->
      ( var
      , (match
           List.find_map_exn b ~f:(fun s ->
               match s with
               | Assign (PName lhs, rhs) when String.equal lhs var -> Some rhs
               | _ -> None)
         with
        (* Success cases *)
        | Num (0 | 1) -> [ Number ]
        | Call (Name "np.zeros", _) -> [ Array ]
        | Name "__emptyList" -> [ List ]
        (* Failure cases *)
        | Num n ->
            raise
              (EarlyCutoff
                 (sprintf
                    "target variable starts as unsupported number %s"
                    (string_of_int n)))
        | Index (_, _) -> raise (EarlyCutoff "target variable starts as index")
        | Call (Name fn, _) ->
            raise
              (EarlyCutoff
                 (sprintf
                    "target variable starts as unsupported function '%s'"
                    fn))
        | Call (_, _) ->
            raise
              (EarlyCutoff "target variable starts as complex function call")
        | Str s ->
            raise
              (EarlyCutoff (sprintf "target variable starts as string '%s'" s))
        | Name n ->
            raise
              (EarlyCutoff (sprintf "target variable starts as name '%s'" n))
        | Hole (_, _) -> failwith "user input has a hole") )
  | _ -> raise (EarlyCutoff "last statement not a variable return")

let solve' : int -> bool -> program -> bool -> (int * program) option =
  Util.Timing_breakdown.record4 Util.Timing_breakdown.Synthesis
  @@ fun depth debug target use_egraphs ->
  let target = canonicalize target in
  let target_var, target_possible_types = infer target in
  let target_loop_vars = loop_vars target in
  let unify : program -> substitutions option =
    if use_egraphs
    then
      Util.Timing_breakdown.record1 Util.Timing_breakdown.Unification
      @@ fun pattern ->
      let graph = Unification.construct_egraph ~target ~debug () in
      Unification.unify_egraph ~graph ~debug ~pattern ()
    else
      Util.Timing_breakdown.record1 Util.Timing_breakdown.Unification
      @@ fun pattern -> Unification.unify_naive ~target ~debug ~pattern ()
  in
  let correct : expr -> expr option =
   fun e ->
    let canonical = canonicalize (np_env, [ Return e ]) in
    (* if String.is_substring ~substring:"randint_size" ([%show: expr] e)
    then (
      Printf.eprintf "%s\n" ([%show: expr] e);
      Printf.eprintf "%s\n" (canonical |> snd |> [%show: block]);
      Printf.eprintf "%s\n" (target |> snd |> [%show: block]);
      Printf.eprintf "-------------------------\n")
    else (); *)
    match unify canonical with
    | Some sub ->
        (match substitute_expr e sub with
        | Some possible_solution ->
            let possible_solution = fix_filter possible_solution in
            if Set.is_empty
                 (Set.inter
                    (referenced_vars_expr possible_solution)
                    (Set.add target_loop_vars target_var))
            then Some possible_solution
            else None
        | None -> None)
    | None -> None
  in
  match
    Cbr_framework.Enumerative_search.top_down
      ~max_iterations:depth
      ~start:
        (target_possible_types
        |> List.map ~f:(fun t -> Hole (t, Util.gensym "hole")))
      ~expand:(expand (referenced_vars target))
      ~correct
  with
  | Some (expansions, ans) -> Some (expansions, (np_env, [ Return (clean ans) ]))
  | None -> None

let solve : int -> ?debug:bool -> program -> bool -> (int * program) option =
 fun depth ?(debug = false) target use_egraphs ->
  solve' depth debug target use_egraphs
