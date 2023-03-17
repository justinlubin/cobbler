open Core.Map
open Lang

let rec substitute_expr : (id * expr) list -> expr -> expr =
 fun binds e ->
  match e with
  | Num n -> Num n
  | Str s -> Str s
  | Index (e1, e2) -> Index (substitute_expr binds e1, substitute_expr binds e2)
  | Call (head, args) ->
      Call (substitute_expr binds head, List.map (substitute_expr binds) args)
  | Name id when List.mem_assoc id binds -> List.assoc id binds
  | Name id -> Name id
  | Hole hole -> Hole hole

and substitute_lhs : (id * expr) list -> lhs -> lhs =
 fun binds lhs ->
  match lhs with
  | Index (new_lhs, e) ->
      Index (substitute_lhs binds new_lhs, substitute_expr binds e)
  | Name id -> Name id

and substitute_stmt : (id * expr) list -> stmt -> (id * expr) list * stmt =
 fun binds stmt ->
  match stmt with
  | Assign (lhs, e) ->
      (match lhs with
      | Name id ->
          (List.remove_assoc id binds, Assign (Name id, substitute_expr binds e))
      | lhs ->
          (binds, Assign (substitute_lhs binds lhs, substitute_expr binds e)))
  | For (id, e, block) ->
      ( List.remove_assoc id binds
      , For (id, substitute_expr binds e, substitute_block binds block) )
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

and inline_lhs : env -> lhs -> block * lhs =
 fun env lhs ->
  match lhs with
  | Name id -> ([], Name id)
  | Index (l, e) ->
      let stmts1, lhs = inline_lhs env l in
      let stmts2, e = inline_expr env e in
      (stmts1 @ stmts2, Index (lhs, e))

let rec inline_stmt : env -> stmt -> block =
 fun env stmt ->
  match stmt with
  | Return e ->
      let stmts, e = inline_expr env e in
      stmts @ [ Return e ]
  | Assign (lhs, e) ->
      let stmts1, lhs = inline_lhs env lhs in
      let stmts2, e = inline_expr env e in
      stmts1 @ stmts2 @ [ Assign (lhs, e) ]
  | For (id, e, block) ->
      let stmts, e = inline_expr env e in
      let body = inline_block env block in
      stmts @ [ For (id, e, body) ]

and inline_block : env -> block -> block =
 fun env block -> List.concat_map (inline_stmt env) block

let inline_program : program -> program =
 fun (env, block) -> (env, inline_block env block)
