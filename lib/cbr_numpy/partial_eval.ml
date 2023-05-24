open Lang

let rec partial_eval_expr : expr -> expr =
 fun e ->
  match e with
  | Num _ | Str _ | Name _ | Hole _ -> e
  | Call (fn, args) ->
      let fn = partial_eval_expr fn in
      (match fn with
      | Name "len" ->
          (match partial_eval_expr (List.hd args) with
          | Call (Name "mul", args)
          | Call (Name "div", args)
          | Call (Name "add", args)
          | Call (Name "eq", args) -> Call (Name "len", [ List.hd args ])
          | Call (Name "ones", args)
          | Call (Name "zeros", args)
          | Call (Name "broadcast", args)
          | Call (Name "fill", _ :: args) -> List.hd args
          | _ -> Call (Name "len", args))
      | _ -> Call (fn, List.map partial_eval_expr args))
  | Index (e1, e2) ->
      (match (e1, e2) with
      | Call (Name "mul", [ x; y ]), e2 ->
          Call
            ( Name "*"
            , [ partial_eval_expr (Index (x, e2))
              ; partial_eval_expr (Index (y, e2))
              ] )
      | Call (Name "div", [ x; y ]), e2 ->
          Call
            ( Name "/"
            , [ partial_eval_expr (Index (x, e2))
              ; partial_eval_expr (Index (y, e2))
              ] )
      | Call (Name "add", [ x; y ]), e2 ->
          Call
            ( Name "+"
            , [ partial_eval_expr (Index (x, e2))
              ; partial_eval_expr (Index (y, e2))
              ] )
      | Call (Name "eq", [ x; y ]), e2 ->
          Call
            ( Name "=="
            , [ partial_eval_expr (Index (x, e2))
              ; partial_eval_expr (Index (y, e2))
              ] )
      | Call (Name "gt", [ x; y ]), e2 ->
          Call
            ( Name ">"
            , [ partial_eval_expr (Index (x, e2))
              ; partial_eval_expr (Index (y, e2))
              ] )
      | Call (Name "zeros", _), _ -> Num 0
      | Call (Name "ones", _), _ -> Num 1
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
 fun block -> List.map partial_eval_stmt block

let partial_eval_program : program -> program =
 fun (env, block) -> (env, partial_eval_block block)
