open Lang

let rec inline_expr : env -> expr -> block * expr =
  fun env e ->
    [], e (* TODO: replace with actual expr inlining *)

and inline_lhs : env -> lhs -> block  * lhs =
  fun env lhs ->
    match lhs with
    | Name id -> [], Name id
    | Index (l, e) ->
        let stmts1, lhs = inline_lhs env l in
        let stmts2, e = inline_expr env e in
        stmts1 @ stmts2, Index(lhs, e)

let inline_stmt : env -> stmt -> block = 
  fun env stmt ->
    match stmt with
    | Return e -> let stmts, e = inline_expr env e in stmts @ [Return e]
    | Assign (lhs, e) -> 
        let stmts1, lhs = inline_lhs env lhs in
        let stmts2, e = inline_expr env e in
          stmts1 @ stmts2 @ [Assign (lhs, e)]
    | _ -> [stmt]

let inline_block : env -> block -> block =
  fun env block ->
    List.concat_map (inline_stmt env) block

let inline_program : program -> program =
  fun (env, block) ->
    (env, inline_block env block)