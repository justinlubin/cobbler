open Core
open Lang

let infix_of_np : string -> string =
 fun s ->
  match s with
  | "np.multiply" -> "*"
  | "np.add" -> "+"
  | "np.subtract" -> "-"
  | "np.power" -> "**"
  | "np.equal" -> "=="
  | "np.greater" -> ">"
  | "np.greater_equal" -> ">="
  | "np.less" -> "<"
  | "np.less_equal" -> "<="
  | _ -> "none"

let rec partial_eval_expr_with_trace : expr -> expr * expr list =
 fun e ->
  match e with
  | Name name ->
      (match String.chop_suffix ~suffix:".size" name with
      | Some name -> 
          let e' = Call (Name "len", [ Name name ]) in e', [e; e']
      | None -> e, [e])
  | Num _ | Str _ | Hole _ -> e, [e]
  | Call (fn, args) ->
      let fn', fn_trace = partial_eval_expr_with_trace fn in
      let args', args_traces = List.map ~f:partial_eval_expr_with_trace args |> List.unzip in
      let g i a =
        let before = List.filteri ~f:(fun j _ -> j < i) args' in
        let after = List.filteri ~f:(fun j _ -> j > i) args in
        Call (fn', before @ (a :: after))
      in
      let old_call_trace = 
        List.map ~f:(fun f -> Call (f, args)) fn_trace 
        @ (List.mapi ~f:(fun j arg_trace -> List.map ~f:(g j) (List.tl_exn arg_trace)) args_traces |> List.concat)
      in
      (match fn' with
      | Name ("np.copy" | "np.tolist") ->
          (match args' with
          | [ arg; amount ] ->
              let e', e'_trace = partial_eval_expr_with_trace (Call (Name "sliceUntil", [ arg; amount ])) in
              e', old_call_trace @ e'_trace
          | _ -> Call (fn, args), old_call_trace)
      | Name "np.ones" ->
          let e', e'_trace = partial_eval_expr_with_trace (Call (Name "np.full", args @ [ Num 1 ])) in
          e', old_call_trace @ e'_trace
      | Name "np.zeros_like" ->
          let e', e'_trace = partial_eval_expr_with_trace (Call (Name "np.zeros", [ Call (Name "len", args) ])) in
          e', old_call_trace @ e'_trace
      | Name "np.arange" -> 
          let e', e'_trace = partial_eval_expr_with_trace (Call (Name "range", args)) in
          e', old_call_trace @ e'_trace
      | Name "-" ->
          (match args' with
          | [ Call (Name "len", [ a ]); offset ] ->
              let e', e'_trace = partial_eval_expr_with_trace (Call (Name "len", [ Call (Name "sliceToEnd", [ a; offset ]) ])) in
              e', old_call_trace @ e'_trace
          | _ -> Call (fn, args), old_call_trace)
      | Name "len" ->
          (match List.hd_exn args' with
          | Call (Name "np.copy", args)
          | Call (Name "np.tolist", args)
          | Call (Name "np.multiply", args)
          | Call (Name "np.divide", args)
          | Call (Name "np.add", args)
          | Call (Name "np.subtract", args)
          | Call (Name "np.power", args)
          | Call (Name "np.equal", args)
          | Call (Name "np.greater", args)
          | Call (Name "np.array_object", args)
          | Call (Call (Name "np.vectorize", [ _; Str "{}" ]), args)
          | Call (Call (Name "np.vectorize", [ _; Str "{1}" ]), _ :: args)
          | Call (Call (Name "np.vectorize", [ _; Str "{2}" ]), args)
          | Call (Name "np.where", args) ->
              let e', e'_trace = partial_eval_expr_with_trace (Call (Name "len", [ List.hd_exn args ])) in
              e', old_call_trace @ e'_trace
          | Call (Name "np.ones", args)
          | Call (Name "np.zeros", args)
          | Call (Name "broadcast", args)
          | Call (Name "np.full", args) -> let e' = List.hd_exn args in e', old_call_trace @ [e']
          | Call (Name "np.random.randint_size", [ _; _; s ]) ->
              s, old_call_trace @ [s]
          | Call (Name "range", [ hd ]) -> hd, old_call_trace @ [hd]
          | _ -> Call (Name "len", args), old_call_trace)
      | Name
          ( "+"
          | "*"
          | "/"
          | "**"
          | "=="
          | "!="
          | ">"
          | ">="
          | "<"
          | "<="
          | "%"
          | "np.random.randint"
          | "np.append" ) -> Call (fn, args), old_call_trace
      | _ ->
          let np_array_object x = Call (Name "np.array_object", [ x ]) in
          (match args with
          | [ Index (arg, i) ] ->
              let e', e'_trace = partial_eval_expr_with_trace
                (Index
                   ( Call
                       ( Call (Name "np.vectorize", [ fn; Str "{}" ])
                       , [ np_array_object arg ] )
                   , i ))
              in
              e', old_call_trace @ e'_trace
          | [ Index (arg1, i); arg2 ] ->
              let e', e'_trace = partial_eval_expr_with_trace
                (Index
                   ( Call
                       ( Call (Name "np.vectorize", [ fn; Str "{2}" ])
                       , [ np_array_object arg1; arg2 ] )
                   , i ))
              in
              e', old_call_trace @ e'_trace
          | [ arg1; Index (arg2, i) ] ->
              let e', e'_trace = partial_eval_expr_with_trace
                (Index
                   ( Call
                       ( Call (Name "np.vectorize", [ fn; Str "{1}" ])
                       , [ arg1; np_array_object arg2 ] )
                   , i ))
              in
              e', old_call_trace @ e'_trace
          | _ -> Call (fn', args'), old_call_trace))
  | Index (e1, e2) ->
      let e1', e1'_trace = partial_eval_expr_with_trace e1 in 
      let e2', e2'_trace = partial_eval_expr_with_trace e2 in
      let old_index_trace = 
        List.map ~f:(fun first -> Index (first, e2)) e1'_trace 
        @ List.map ~f:(fun second -> Index (e1', second)) (List.tl_exn e2'_trace)
      in
      (match (e1', e2') with
      | _, Call (Name "+", [ base; Num offset ])
      | _, Call (Name "+", [ Num offset; base ]) ->
          let e', e'_trace = partial_eval_expr_with_trace
            (Index (Call (Name "sliceToEnd", [ e1; Num offset ]), base))
          in
          e', old_index_trace @ e'_trace
      | Call (Name "np.zeros", _), _ -> Num 0, old_index_trace @ [Num 0]
      | Call (Name "np.full", [ _; v ]), _ -> v, old_index_trace @ [v]
      | Call (Name "range", [ _ ]), i -> i, old_index_trace @ [i]
      | Call (Name "np.random.randint_size", [ low; high; _ ]), i ->
          let e', e'_trace = partial_eval_expr_with_trace (Call (Name "np.random.randint", [ low; high ])) in
          e', old_index_trace @ e'_trace
      | Call (Name fn_name, [ x; y ]), _ when String.equal "none" (infix_of_np fn_name) ->
          Index (e1', e2'), old_index_trace
      | Call (Name fn_name, [ x; y ]), _ ->
          let e', e'_trace = partial_eval_expr_with_trace
          (Call
            ( Name (infix_of_np fn_name)
            , [ Index (x, e2')
              ; Index (y, e2')
              ] ))
          in
          e', old_index_trace @ e'_trace
      | _ ->
          (* print_endline ("e1: " ^ (Parse.sexp_of_expr e1 |> Core.Sexp.to_string)); *)
          Index (e1', e2'), old_index_trace)

and partial_eval_pat_with_trace : pat -> pat * pat list =
 fun pat ->
  match pat with
  | PName _ | PHole _ -> pat, [pat]
  | PIndex (l, e) -> 
      let e', e'_trace = partial_eval_expr_with_trace e in
      let l', l'_trace = partial_eval_pat_with_trace l in
      let pat_trace =
        List.map ~f:(fun ex -> PIndex (l, ex)) e'_trace
        @ List.map ~f:(fun lhs -> PIndex (lhs, e')) (List.tl_exn l'_trace)
      in
      PIndex (l', e'), pat_trace

let rec substitute_in_expr : id -> expr -> expr -> expr =
 fun lhs rhs e ->
  match e with
  (* Main case *)
  | Name x when String.equal lhs x -> rhs
  | Name x -> Name x
  (* Recursive cases *)
  | Index (e1, e2) ->
      Index (substitute_in_expr lhs rhs e1, substitute_in_expr lhs rhs e2)
  | Call (head, args) ->
      Call
        ( substitute_in_expr lhs rhs head
        , List.map ~f:(substitute_in_expr lhs rhs) args )
  (* Base cases *)
  | Num _ | Str _ | Hole (_, _) -> e

(* Note: not capture-avoiding! *)
let rec substitute_in_block : id -> expr -> block -> block =
 fun lhs rhs b ->
  b
  |> List.fold_left ~init:(true, []) ~f:(fun (should_continue, acc) stmt ->
         if should_continue
         then (
           match stmt with
           | Assign (PName alhs, arhs) when String.equal lhs alhs ->
               ( false
               , Assign (PName alhs, substitute_in_expr lhs rhs arhs) :: acc )
           | Assign (alhs, arhs) ->
               (true, Assign (alhs, substitute_in_expr lhs rhs arhs) :: acc)
           | For (PName i, e, body) when String.equal lhs i ->
               (true, For (PName i, substitute_in_expr lhs rhs e, body) :: acc)
           | For (i, e, body) ->
               ( true
               , For
                   ( i
                   , substitute_in_expr lhs rhs e
                   , substitute_in_block lhs rhs body )
                 :: acc )
           | Return e -> (true, Return (substitute_in_expr lhs rhs e) :: acc)
           | If (cond, then_branch, else_branch) ->
               ( true
               , If
                   ( substitute_in_expr lhs rhs cond
                   , substitute_in_block lhs rhs then_branch
                   , substitute_in_block lhs rhs else_branch )
                 :: acc ))
         else (false, stmt :: acc))
  |> snd
  |> List.rev

let rec partial_eval_stmt_with_trace : stmt -> stmt * stmt list =
 fun stmt ->
  match stmt with
  | Assign (pat, e) -> 
      let pat', pat_trace = partial_eval_pat_with_trace pat in
      let e', e_trace = partial_eval_expr_with_trace e in
      Assign (pat', e'), 
      List.map ~f:(fun p -> Assign (p, e)) pat_trace 
      @ List.map ~f:(fun ex -> Assign (pat', ex)) (List.tl_exn e_trace)
  | For (id, e, body) ->
      let e', e_trace = partial_eval_expr_with_trace e in
      let body', body_trace = partial_eval_block_with_trace body in
      let trace = List.map ~f:(fun ex -> For (id, ex, body)) e_trace @
        List.map ~f:(fun b -> For (id, e', b)) (List.tl_exn body_trace) in
      (match e' with
      | Call (Name "range", [ _ ]) as e' -> 
          For (id, e', body'), trace 
      | e' -> 
          (match id with
          | PName x ->
              let i = Util.gensym ("i_" ^ x) in
              let stmt', stmt_trace = partial_eval_stmt_with_trace
                (For
                  ( PName i
                  , Call (Name "range", [ Call (Name "len", [ e ]) ])
                  , substitute_in_block x (Index (e, Name i)) body ))
              in
              stmt', trace @ stmt_trace
          | PIndex _ | PHole _ -> For (id, e', body'), trace))
  | Return e -> 
      let e', e_trace = partial_eval_expr_with_trace e in 
      Return e', List.map ~f:(fun ex -> Return ex) e_trace
  | If (cond, body, orelse) ->
      let cond', cond_trace = partial_eval_expr_with_trace cond in
      let body', body_trace = partial_eval_block_with_trace body in
      let orelse', orelse_trace = partial_eval_block_with_trace orelse in
      If (cond', body', orelse'),
      List.map ~f:(fun c -> If (c, body, orelse)) cond_trace @
      List.map ~f:(fun b -> If (cond', b, orelse)) body_trace @
      List.map ~f:(fun oe -> If (cond', body', oe)) orelse_trace

and partial_eval_block_with_trace : block -> block * block list =
 fun block ->
  let stmts, stmt_traces = List.map ~f:partial_eval_stmt_with_trace block |> List.unzip in
  let f i stmt =
    let before = List.filteri ~f:(fun j _ -> j < i) stmts in
    let after = List.filteri ~f:(fun j _ -> j > i) block in
    before @ (stmt :: after)
  in
  stmts, block :: (List.mapi ~f:(fun j stmt_trace -> List.map ~f:(f j) (List.tl_exn stmt_trace)) stmt_traces |> List.concat)

let partial_eval_program_with_trace : program * program list -> program * program list =
 fun ((env, block), t) -> 
  let block', block_trace = partial_eval_block_with_trace block in
  (env, block'), t @ List.tl_exn (List.map ~f:(fun x -> (env, x)) block_trace)
