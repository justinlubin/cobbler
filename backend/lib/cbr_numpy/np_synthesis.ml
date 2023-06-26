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
  | List -> make_grammar_entries fvs [ ("np.tolist", [], [ Array ]) ]
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
        ; ("np.full", [], [ Number; Constant ])
        ; ("np.greater", [ ">" ], [ Array; Array ])
        ; ("np.where", [], [ Array; Array; Array ])
        ; ("np.roll", [], [ Array; Number ])
        ; ("np.convolve_valid", [], [ Array; Array ])
        ; ( "np.random.randint_size"
          , [ "np.random.randint" ]
          , [ Number; Number; Number ] )
        ; ("range", [], [ Number ])
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

exception IncorrectSubstitutionType

let rec binding_ok : hole_type -> expr -> bool =
 fun tau e ->
  match tau with
  | Constant ->
      (match e with
      | Num _ | Str _ | Name _ | Hole (_, _) -> true
      | Index (e1, e2) -> binding_ok tau e1 && binding_ok tau e2
      | Call (_, _) -> false)
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
            else raise IncorrectSubstitutionType
        | None -> e')
  in
  try Some (substitute e) with
  | IncorrectSubstitutionType -> None

let canonicalize : program -> program =
 fun p -> p |> Inline.inline_program |> Partial_eval.partial_eval_program

let rec simplify : expr -> expr =
 fun e ->
  match e with
  | Index (e1, e2) -> Index (simplify e1, simplify e2)
  | Call (fn, args) ->
      let fn = simplify fn in
      let args = List.map ~f:simplify args in
      (match (fn, args) with
      (* Slicing *)
      | Name "len", [ Call (Name "sliceToEnd", [ a; x ]) ]
      | Name "len", [ Call (Name "sliceUntil", [ a; x ]) ] ->
          simplify (Call (Name "-", [ Call (Name "len", [ a ]); x ]))
      | ( Name "sliceToEnd"
        , [ a; Call (Name "-", [ Call (Name "len", [ a' ]); x ]) ] )
        when [%eq: expr] a a' ->
          simplify
            (Call (Name "sliceToEnd", [ a; Call (Name "negate", [ x ]) ]))
      | Name "sliceToEnd", [ a; Num 0 ] -> a
      | Name "sliceToEnd", [ Call (Name "broadcast", [ n ]); _ ] ->
          Call (Name "broadcast", [ n ])
      | Name "sliceUntil", [ Call (Name "np.full", [ _; v ]); x ] ->
          simplify (Call (Name "np.full", [ x; v ]))
      | Name "sliceUntil", [ Call (Name "range", [ _ ]); x ] ->
          simplify (Call (Name "range", [ x ]))
      | ( Name "sliceUntil"
        , [ a; Call (Name "-", [ Call (Name "len", [ a' ]); x ]) ] )
        when [%eq: expr] a a' ->
          simplify
            (Call (Name "sliceUntil", [ a; Call (Name "negate", [ x ]) ]))
      | Name "sliceUntil", [ a; Call (Name "len", [ a' ]) ]
        when [%eq: expr] a a' -> a
      | Name "sliceUntil", [ Call (Name "broadcast", [ n ]); _ ] ->
          Call (Name "broadcast", [ n ])
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
                  | "np.tolist"
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
          | Name "np.greater" )
        , [ arg1; arg2 ] ) ->
          Call
            ( fn
            , [ arg1
              ; Call (Name "sliceUntil", [ arg2; Call (Name "len", [ arg1 ]) ])
              ] )
      | _ -> Call (fn, args))
  | Num _ | Str _ | Name _ | Hole (_, _) -> e

let clean : expr -> expr = fun e -> simplify (cap_second_arguments e)

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

let possible_types : program -> hole_type list =
 fun (_, b) ->
  match List.last_exn b with
  | Return (Name var) ->
      (match
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
          raise (EarlyCutoff "target variable starts as complex function call")
      | Str s ->
          raise
            (EarlyCutoff (sprintf "target variable starts as string '%s'" s))
      | Name n ->
          raise (EarlyCutoff (sprintf "target variable starts as name '%s'" n))
      | Hole (_, _) -> failwith "user input has a hole")
  | _ -> failwith "first statement not an assignment"

let solve : int -> ?debug:bool -> program -> bool -> program option =
 fun depth ?(debug = false) target use_egraphs ->
  let target = canonicalize target in
  let target_loop_vars = loop_vars target in
  let unify : pattern:program -> substitutions option =
    if use_egraphs
    then (
      let graph = Unification.construct_egraph ~target ~debug () in
      Unification.unify_egraph ~graph ~debug ())
    else Unification.unify_naive ~target ~debug ()
  in
  let correct : expr -> expr option =
   fun e ->
    let canonical = canonicalize (np_env, [ Return e ]) in
    (* if String.is_substring ~substring:"np.convolve_valid" ([%show: expr] e)
       && String.is_substring ~substring:"np.full" ([%show: expr] e)
       && String.is_substring
            ~substring:"window_size"
            ([%show: block] (snd target))
            h
            :noh

    then (
      Printf.eprintf "%s\n" ([%show: expr] e);
      Printf.eprintf "%s\n" (canonical |> snd |> [%show: block]);
      Printf.eprintf "%s\n" (target |> snd |> [%show: block]);
      Printf.eprintf "-------------------------\n")
    else (); *)
    match unify ~pattern:canonical with
    | Some sub ->
        (match substitute_expr e sub with
        | Some possible_solution ->
            if Set.is_empty
                 (Set.inter
                    (referenced_vars_expr possible_solution)
                    target_loop_vars)
            then Some possible_solution
            else None
        | None -> None)
    | None -> None
  in
  match
    Cbr_framework.Enumerative_search.top_down
      ~max_iterations:depth
      ~start:
        (target
        |> possible_types
        |> List.map ~f:(fun t -> Hole (t, Util.gensym "hole")))
      ~expand:(expand (referenced_vars target))
      ~correct
  with
  | Some ans -> Some (np_env, [ Return (clean ans) ])
  | None -> None
