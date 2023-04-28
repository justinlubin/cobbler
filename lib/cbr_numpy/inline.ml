open Core.Map
open Lang

let rec substitute_expr : (id * expr) list -> expr -> expr =
 fun binds e ->
  match e with
  | Num _ | Str _ | Hole _ -> e
  | Index (e1, e2) -> Index (substitute_expr binds e1, substitute_expr binds e2)
  | Call (head, args) ->
      Call (substitute_expr binds head, List.map (substitute_expr binds) args)
  | Name id when List.mem_assoc id binds -> List.assoc id binds
  | Name id -> Name id

and substitute_pat : (id * expr) list -> pat -> pat =
 fun binds pat ->
  match pat with
  | PIndex (new_pat, e) ->
      PIndex (substitute_pat binds new_pat, substitute_expr binds e)
  | PName id -> PName id
  | PHole _ -> pat

and substitute_stmt : (id * expr) list -> stmt -> (id * expr) list * stmt =
 fun binds stmt ->
  match stmt with
  | Assign (pat, e) ->
      (match pat with
      | PName id ->
          ( List.remove_assoc id binds
          , Assign (PName id, substitute_expr binds e) )
      | pat ->
          (binds, Assign (substitute_pat binds pat, substitute_expr binds e)))
  | For (pat, e, block) ->
      let new_binds =
        match pat with
        | PName id -> List.remove_assoc id binds
        | pat -> binds
      in
      ( new_binds
      , For (pat, substitute_expr binds e, substitute_block new_binds block) )
  | Return e -> (binds, Return (substitute_expr binds e))

and substitute_block : (id * expr) list -> block -> block =
 fun binds body ->
  let _, block = List.fold_left_map substitute_stmt binds body in
  block

let substitute : expr list -> id list -> block -> block * expr =
 fun args params body ->
  let block = substitute_block (List.combine params args) body in
  match List.rev block with
  | Return e :: rev_body -> (List.rev rev_body, e)
  | _ -> failwith "return statement not found in function"

let rec inline_expr : env -> expr -> block * expr =
 fun env e ->
  match e with
  | Call (head, args) ->
      (match head with
      | Name fn ->
          (match find env fn with
          | Some (params, body) -> substitute args params body
          | None -> ([], Call (head, args)))
      | _ -> ([], e))
  | _ -> ([], e)

and inline_pat : env -> pat -> block * pat =
 fun env pat ->
  match pat with
  | PName _ | PHole _ -> ([], pat)
  | PIndex (lhs, e) ->
      let stmts1, new_pat = inline_pat env lhs in
      let stmts2, e = inline_expr env e in
      (stmts1 @ stmts2, PIndex (new_pat, e))

let rec inline_stmt : env -> stmt -> block =
 fun env stmt ->
  match stmt with
  | Return e ->
      let stmts, e = inline_expr env e in
      stmts @ [ Return e ]
  | Assign (pat, e) ->
      let stmts1, new_pat = inline_pat env pat in
      let stmts2, e = inline_expr env e in
      stmts1 @ stmts2 @ [ Assign (new_pat, e) ]
  | For (id, e, block) ->
      let stmts, e = inline_expr env e in
      let body = inline_block env block in
      stmts @ [ For (id, e, body) ]

and inline_block : env -> block -> block =
 fun env block -> List.concat_map (inline_stmt env) block

let inline_program : program -> program =
 fun (env, block) -> (env, inline_block env block)
