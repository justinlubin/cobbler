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
        [ Call (Name "sum", [ Hole (Array, Util.gensym "hole") ]) ]
    | Hole (Array, _) ->
        [ Call
            ( Name "mul"
            , [ Hole (Array, Util.gensym "hole")
              ; Hole (Array, Util.gensym "hole")
              ] )
        ; Call
            ( Name "add"
            , [ Hole (Array, Util.gensym "hole")
              ; Hole (Array, Util.gensym "hole")
              ] )
        ; Call
            ( Name "div"
            , [ Hole (Array, Util.gensym "hole")
              ; Hole (Array, Util.gensym "hole")
              ] )
        ; Call
            ( Name "eq"
            , [ Hole (Array, Util.gensym "hole")
              ; Hole (Array, Util.gensym "hole")
              ] )
        ; Call (Name "ones", [ Hole (Number, Util.gensym "hole") ])
        ; Call
            ( Name "gt"
            , [ Hole (Array, Util.gensym "hole")
              ; Hole (Array, Util.gensym "hole")
              ] )
        ; Call
            ( Name "where"
            , [ Hole (Array, Util.gensym "hole")
              ; Hole (Array, Util.gensym "hole")
              ; Hole (Array, Util.gensym "hole")
              ] )
        ; Call
            ( Name "roll"
            , [ Hole (Array, Util.gensym "hole")
              ; Hole (Number, Util.gensym "hole")
              ] )
        ; Call
            ( Name "convolve_valid"
            , [ Hole (Array, Util.gensym "hole")
              ; Hole (Array, Util.gensym "hole")
              ] )
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

let solve : int -> ?debug:bool -> hole_type -> program -> bool -> program option
  =
 fun depth ?(debug = false) program_type target use_egraphs ->
  let unify : pattern:program -> substitutions option =
    if use_egraphs
    then (
      let graph = Unification.construct_egraph ~target ~debug () in
      Unification.unify_egraph ~graph ~debug ())
    else Unification.unify_naive ~target ~debug ()
  in
  let correct : expr -> expr option =
   fun e ->
    (* if debug then print_endline (Parse.sexp_of_expr e |> Sexp.to_string) else (); *)
    let canonical = canonicalize (np_env, [ Return e ]) in
    match unify ~pattern:canonical with
    | Some sub -> Some (substitute_expr e sub)
    | None -> None
  in
  match
    Cbr_framework.Enumerative_search.top_down
      ~max_iterations:depth
      ~start:(Hole (program_type, Util.gensym "hole"))
      ~expand
      ~correct
  with
  | Some ans -> Some (np_env, [ Return ans ])
  | None -> None
