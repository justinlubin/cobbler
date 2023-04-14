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
          | Call (Name "mul", mul_args) ->
              Call (Name "len", [ List.hd mul_args ])
          | _ -> Call (Name "len", args))
      | _ -> Call (fn, List.map partial_eval_expr args))
  | Index (e1, e2) ->
      let e1 = partial_eval_expr e1 in
      let e2 = partial_eval_expr e2 in
      (match (e1, e2) with
      | Call (Name "mul", [ x; y ]), Num i ->
          Call (Name "*", [ Index (x, Num i); Index (y, Num i) ])
      | _ -> Index (e1, e2))

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

and partial_eval_block : block -> block =
 fun block -> List.map partial_eval_stmt block

let partial_eval_program : program -> program =
 fun (env, block) -> (env, partial_eval_block block)
