open Core.Map
open Lang

let rec substitute_expr : (id * expr) list -> expr -> expr =
  fun binds e ->
    match e with
    | Index (e1, e2) -> Index (substitute_expr binds e1, substitute_expr binds e2)
    | Call (e, e_list) -> Call (substitute_expr binds e, List.map (substitute_expr binds) e_list)
    | Name id when (List.mem_assoc id binds) -> List.assoc id binds
    | _ -> e
    
and substitute_lhs : (id * expr) list -> lhs -> lhs =
  fun binds lhs ->
    match lhs with
    | Index (lhs, e) -> Index (substitute_lhs binds lhs, substitute_expr binds e)
    | Name id -> Name id

and substitute_stmt : (id * expr) list -> stmt -> stmt =
  fun binds stmt ->
    match stmt with
    | Assign (lhs, e) -> Assign (substitute_lhs binds lhs, substitute_expr binds e)
    | For (id, e, block) -> For (id, substitute_expr binds e, substitute_block binds block)
    | Return e -> Return (substitute_expr binds e)

and substitute_block : (id * expr) list -> block -> block =
  fun binds block ->
    List.map (substitute_stmt binds) block 

let substitute : expr list -> id list -> block -> block * expr =
  fun args params body ->
    let block = substitute_block (List.combine params args) body in
    match List.rev block with
    | Return e :: rev -> List.rev rev, e
    | _ -> failwith "return statement not found in function"

let rec inline_expr : env -> expr -> block * expr =
  fun env e ->
    match e with
    | Call (e, args) ->
      ( match e with
        | Name fn -> 
          ( match find env fn with
            | Some (params, body) -> substitute args params body
            | None -> [], e
          )
        | _ -> [], e)
    | _ -> [], e

and inline_lhs : env -> lhs -> block * lhs =
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