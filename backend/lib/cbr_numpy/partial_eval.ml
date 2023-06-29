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
      | Name ("np.copy" | "np.tolist") ->
          (match args with
          | [ arg; amount ] ->
              partial_eval_expr (Call (Name "sliceUntil", [ arg; amount ]))
          | _ -> Call (fn, args))
      | Name "np.ones" ->
          partial_eval_expr (Call (Name "np.full", args @ [ Num 1 ]))
      | Name "np.zeros_like" ->
          partial_eval_expr
            (Call (Name "np.zeros", [ Call (Name "len", args) ]))
      | Name "np.arange" -> partial_eval_expr (Call (Name "range", args))
      | Name "-" ->
          (match args with
          | [ Call (Name "len", [ a ]); offset ] ->
              partial_eval_expr
                (Call (Name "len", [ Call (Name "sliceToEnd", [ a; offset ]) ]))
          | _ -> Call (fn, args))
      | Name "len" ->
          (match partial_eval_expr (List.hd_exn args) with
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
              partial_eval_expr (Call (Name "len", [ List.hd_exn args ]))
          | Call (Name "np.ones", args)
          | Call (Name "np.zeros", args)
          | Call (Name "broadcast", args)
          | Call (Name "np.full", args) -> partial_eval_expr (List.hd_exn args)
          | Call (Name "np.random.randint_size", [ _; _; s ]) ->
              partial_eval_expr s
          | Call (Name "range", [ hd ]) -> partial_eval_expr hd
          | _ -> Call (Name "len", args))
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
          | "np.append" ) -> Call (fn, args)
      | _ ->
          let np_array_object x = Call (Name "np.array_object", [ x ]) in
          (match args with
          | [ Index (arg, i) ] ->
              partial_eval_expr
                (Index
                   ( Call
                       ( Call (Name "np.vectorize", [ fn; Str "{}" ])
                       , [ np_array_object arg ] )
                   , i ))
          | [ Index (arg1, i); arg2 ] ->
              partial_eval_expr
                (Index
                   ( Call
                       ( Call (Name "np.vectorize", [ fn; Str "{2}" ])
                       , [ np_array_object arg1; arg2 ] )
                   , i ))
          | [ arg1; Index (arg2, i) ] ->
              partial_eval_expr
                (Index
                   ( Call
                       ( Call (Name "np.vectorize", [ fn; Str "{1}" ])
                       , [ arg1; np_array_object arg2 ] )
                   , i ))
          | _ -> Call (fn, args)))
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
      | Call (Name "np.subtract", [ x; y ]), e2 ->
          Call
            ( Name "-"
            , [ partial_eval_expr (Index (x, e2))
              ; partial_eval_expr (Index (y, e2))
              ] )
      | Call (Name "np.power", [ x; y ]), e2 ->
          Call
            ( Name "**"
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
      | Call (Name "np.greater_equal", [ x; y ]), e2 ->
          Call
            ( Name ">="
            , [ partial_eval_expr (Index (x, e2))
              ; partial_eval_expr (Index (y, e2))
              ] )
      | Call (Name "np.less", [ x; y ]), e2 ->
          Call
            ( Name "<"
            , [ partial_eval_expr (Index (x, e2))
              ; partial_eval_expr (Index (y, e2))
              ] )
      | Call (Name "np.less_equal", [ x; y ]), e2 ->
          Call
            ( Name "<="
            , [ partial_eval_expr (Index (x, e2))
              ; partial_eval_expr (Index (y, e2))
              ] )
      | Call (Name "np.zeros", _), _ -> Num 0
      | Call (Name "np.full", [ _; v ]), _ -> partial_eval_expr v
      | Call (Name "range", [ _ ]), i -> partial_eval_expr i
      | Call (Name "np.random.randint_size", [ low; high; _ ]), i ->
          partial_eval_expr (Call (Name "np.random.randint", [ low; high ]))
      | _ ->
          (* print_endline ("e1: " ^ (Parse.sexp_of_expr e1 |> Core.Sexp.to_string)); *)
          Index (e1, e2))

and partial_eval_pat : pat -> pat =
 fun pat ->
  match pat with
  | PName _ | PHole _ -> pat
  | PIndex (l, e) -> PIndex (partial_eval_pat l, partial_eval_expr e)

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

let rec partial_eval_stmt : stmt -> stmt =
 fun stmt ->
  match stmt with
  | Assign (pat, e) -> Assign (partial_eval_pat pat, partial_eval_expr e)
  | For (id, e, body) ->
      (match partial_eval_expr e with
      | Call (Name "range", [ _ ]) as e' -> For (id, e', partial_eval_block body)
      | e' ->
          (match id with
          | PName x ->
              let i = Util.gensym ("i_" ^ x) in
              partial_eval_stmt
                (For
                   ( PName i
                   , Call (Name "range", [ Call (Name "len", [ e ]) ])
                   , substitute_in_block x (Index (e, Name i)) body ))
          | _ -> For (id, e', partial_eval_block body)))
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
