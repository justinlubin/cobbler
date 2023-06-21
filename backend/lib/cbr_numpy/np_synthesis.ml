open Lang
open Env
open Core

let expand : int -> expr -> expr list =
 fun _ e ->
  let rec expand' = function
    | Num n -> [ Num n ]
    | Str s -> [ Str s ]
    | Name id -> [ Name id ]
    | Hole (Number, _) ->
        [ Call (Name "np.sum", [ Hole (Array, Util.gensym "hole") ]) ]
    | Hole (Array, _) ->
        [ Call
            ( Name "np.multiply"
            , [ Hole (Array, Util.gensym "hole")
              ; Hole (Array, Util.gensym "hole")
              ] )
        ; Call
            ( Name "np.divide"
            , [ Hole (Array, Util.gensym "hole")
              ; Hole (Array, Util.gensym "hole")
              ] )
        ; Call
            ( Name "np.add"
            , [ Hole (Array, Util.gensym "hole")
              ; Hole (Array, Util.gensym "hole")
              ] )
        ; Call
            ( Name "np.subtract"
            , [ Hole (Array, Util.gensym "hole")
              ; Hole (Array, Util.gensym "hole")
              ] )
        ; Call
            ( Name "np.equal"
            , [ Hole (Array, Util.gensym "hole")
              ; Hole (Array, Util.gensym "hole")
              ] )
        ; Call (Name "np.ones", [ Hole (Number, Util.gensym "hole") ])
        ; Call
            ( Name "np.greater"
            , [ Hole (Array, Util.gensym "hole")
              ; Hole (Array, Util.gensym "hole")
              ] )
        ; Call
            ( Name "np.where"
            , [ Hole (Array, Util.gensym "hole")
              ; Hole (Array, Util.gensym "hole")
              ; Hole (Array, Util.gensym "hole")
              ] )
        ; Call
            ( Name "np.roll"
            , [ Hole (Array, Util.gensym "hole")
              ; Hole (Number, Util.gensym "hole")
              ] )
        ; Call
            ( Name "np.convolve_valid"
            , [ Hole (Array, Util.gensym "hole")
              ; Hole (Array, Util.gensym "hole")
              ] )
        ; Call
            ( Name "np.random.randint_size"
            , [ Hole (Number, Util.gensym "hole")
              ; Hole (Number, Util.gensym "hole")
              ; Hole (Number, Util.gensym "hole")])
        ]
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
          (* only handles unary 2-ary, and 3-ary function calls *)
          | _ -> []
        in
        List.map ~f:(fun fn' -> Call (fn', args)) expanded_fn
        @ List.map ~f:(fun args' -> Call (fn, args')) expanded_args
  in
  expand' e

let substitute_expr : expr -> substitutions -> expr =
 fun e subs ->
  let rec substitute = function
    | Num n -> Num n
    | Str s -> Str s
    | Name id -> Name id
    | Index (hd, index) -> Index (substitute hd, substitute index)
    | Call (fn, args) -> Call (substitute fn, List.map ~f:substitute args)
    | Hole (_, h) -> Map.find_exn subs h
  in
  substitute e

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
                  | "np.equal"
                  | "np.greater"
                  | "np.where" )
              , args' )
          ] ) -> simplify (Call (Name "len", [ List.hd_exn args' ]))
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

let solve : int -> ?debug:bool -> program -> bool -> program option =
 fun depth ?(debug = false) target use_egraphs ->
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
    (* if String.is_substring ~substring:"subtract" ([%show: expr] e)
    then (
      Printf.eprintf "%s\n" ([%show: expr] e);
      Printf.eprintf "%s\n" (canonical |> snd |> [%show: block]);
      Printf.eprintf "%s\n" (target |> snd |> [%show: block]))
    else (); *)
    match unify ~pattern:canonical with
    | Some sub -> Some (substitute_expr e sub)
    | None -> None
  in
  match
    Cbr_framework.Enumerative_search.top_down
      ~max_iterations:depth
      ~start:
        [ Hole (Array, Util.gensym "hole"); Hole (Number, Util.gensym "hole") ]
      ~expand
      ~correct
  with
  | Some ans -> Some (np_env, [ Return (clean ans) ])
  | None -> None
