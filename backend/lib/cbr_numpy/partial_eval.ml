open Core
open Lang

let rec partial_eval_expr : expr -> expr =
 fun e ->
  match e with
  | Name name ->
      (match String.chop_suffix ~suffix:".size" name with
      | Some name -> Call (Name "len", [ Name name ])
      | None -> Name name)
  | Num _ | Str _ | Hole _ -> e
  | Call (fn, args) ->
      let fn = partial_eval_expr fn in
      let args = List.map ~f:partial_eval_expr args in
      (match fn with
      | Name "np.zeros_like" ->
          partial_eval_expr
            (Call (Name "np.zeros", [ Call (Name "len", args) ]))
      | Name "np.arange" -> partial_eval_expr (Call (Name "range", args))
      | Name "-" ->
          (match args with
          | [ Call (Name "len", [ a ]); Num offset ] ->
              partial_eval_expr
                (Call
                   (Name "len", [ Call (Name "sliceToEnd", [ a; Num offset ]) ]))
          | _ -> Call (Name "-", List.map ~f:partial_eval_expr args))
      | Name "len" ->
          (match partial_eval_expr (List.hd_exn args) with
          | Call (Name "np.multiply", args)
          | Call (Name "np.divide", args)
          | Call (Name "np.add", args)
          | Call (Name "np.subtract", args)
          | Call (Name "np.equal", args)
          | Call (Name "np.greater", args)
          | Call (Name "np.tolist", args)
          | Call (Name "np.where", args) ->
              partial_eval_expr (Call (Name "len", [ List.hd_exn args ]))
          | Call (Name "np.ones", args)
          | Call (Name "range", args)
          | Call (Name "np.zeros", args)
          | Call (Name "broadcast", args)
          | Call (Name "fill", _ :: args) ->
              partial_eval_expr (List.hd_exn args)
          | Call (Name "np.random.randint_size", [ _; _; s ]) ->
              partial_eval_expr s
          | _ -> Call (Name "len", args))
      | _ -> Call (fn, args))
  | Index (e1, e2) ->
      (match (e1, e2) with
      | _, Call (Name "+", [ base; Num offset ])
      | _, Call (Name "+", [ Num offset; base ]) ->
          partial_eval_expr
            (Index (Call (Name "sliceToEnd", [ e1; Num offset ]), base))
      | Call (Name "np.multiply", [ x; y ]), e2 ->
          Call
            ( Name "*"
            , [ partial_eval_expr (Index (x, e2))
              ; partial_eval_expr (Index (y, e2))
              ] )
      | Call (Name "np.divide", [ x; y ]), e2 ->
          Call
            ( Name "/"
            , [ partial_eval_expr (Index (x, e2))
              ; partial_eval_expr (Index (y, e2))
              ] )
      | Call (Name "np.add", [ x; y ]), e2 ->
          Call
            ( Name "+"
            , [ partial_eval_expr (Index (x, e2))
              ; partial_eval_expr (Index (y, e2))
              ] )
      | Call (Name "np.equal", [ x; y ]), e2 ->
          Call
            ( Name "=="
            , [ partial_eval_expr (Index (x, e2))
              ; partial_eval_expr (Index (y, e2))
              ] )
      | Call (Name "np.greater", [ x; y ]), e2 ->
          Call
            ( Name ">"
            , [ partial_eval_expr (Index (x, e2))
              ; partial_eval_expr (Index (y, e2))
              ] )
      | Call (Name "np.zeros", _), _ -> Num 0
      | Call (Name "np.ones", _), _ -> Num 1
      | Call (Name "range", _), i -> partial_eval_expr i
      | _ ->
          (* print_endline ("e1: " ^ (Parse.sexp_of_expr e1 |> Core.Sexp.to_string)); *)
          Index (e1, e2))

and partial_eval_pat : pat -> pat =
 fun pat ->
  match pat with
  | PName _ | PHole _ -> pat
  | PIndex (l, e) -> PIndex (partial_eval_pat l, partial_eval_expr e)

let rec partial_eval_stmt : stmt -> stmt =
 fun stmt ->
  match stmt with
  | Assign (pat, e) -> Assign (partial_eval_pat pat, partial_eval_expr e)
  | For (id, e, body) -> For (id, partial_eval_expr e, partial_eval_block body)
  | Return e -> Return (partial_eval_expr e)
  | If (cond, body, orelse) ->
      If
        ( partial_eval_expr cond
        , partial_eval_block body
        , partial_eval_block orelse )

and partial_eval_block : block -> block =
 fun block -> List.map ~f:partial_eval_stmt block

let partial_eval_program : program -> program =
 fun (env, block) -> (env, partial_eval_block block)
