open Lang
open Env

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
        ]
    | Index (head, index) ->
        let expanded_head = expand' head in
        let expanded_index = expand' index in
        List.map (fun h : expr -> Index (h, index)) expanded_head
        @ List.map (fun i : expr -> Index (head, i)) expanded_index
    | Call (fn, args) ->
        let expanded_fn = expand' fn in
        let expanded_args =
          match args with
          | [ e1 ] -> List.map (fun e' -> [ e' ]) (expand' e1)
          | [ e1; e2 ] ->
              let expanded_e1 = expand' e1 in
              let expanded_e2 = expand' e2 in
              List.map (fun e' -> [ e'; e2 ]) expanded_e1
              @ List.map (fun e' -> [ e1; e' ]) expanded_e2
          (* only handles unary and 2-ary function calls *)
          | _ -> []
        in
        List.map (fun fn' -> Call (fn', args)) expanded_fn
        @ List.map (fun args' -> Call (fn, args')) expanded_args
  in
  expand' e

let substitute_expr : expr -> substitutions -> expr =
 fun e subs ->
  let rec substitute = function
    | Num n -> Num n
    | Str s -> Str s
    | Name id -> Name id
    | Index (hd, index) -> Index (substitute hd, substitute index)
    | Call (fn, args) -> Call (substitute fn, List.map substitute args)
    | Hole (_, h) -> Core.Map.find_exn subs h
  in
  substitute e

let canonicalize : program -> program =
 fun p -> p |> Inline.inline_program |> Partial_eval.partial_eval_program

let solve : int -> hole_type -> program -> program option =
 fun depth program_type target ->
  let correct : expr -> expr option =
   fun e ->
    let canonical = canonicalize (np_env, [ Return e ]) in
    match Unification.unify_naive ~target ~pattern:canonical with
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
