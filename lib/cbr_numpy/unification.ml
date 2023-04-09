open Lang
open Core
open Parse
open Ego.Generic

let unique_hole_fail ~key:_ _ _ = failwith "Non-unique hole"

let merge_option_skewed
    : substitutions option -> substitutions option -> substitutions option
  =
 fun sub1 sub2 ->
  match (sub1, sub2) with
  | Some s1, Some s2 ->
      Some (String.Map.merge_skewed s1 s2 ~combine:unique_hole_fail)
  | _ -> None

let rec unify_expr : expr -> expr -> substitutions option =
 fun expr1 expr2 ->
  match expr2 with
  | Hole h ->
      (match expr1 with
      | _ -> Some (String.Map.of_alist_exn [ (h, expr1) ]))
  | Num n2 ->
      (match expr1 with
      | Num n1 when Int.equal n1 n2 -> Some String.Map.empty
      | _ -> None)
  | Index (index2, iter2) ->
      (match expr1 with
      | Index (index1, iter1) ->
          let sub1 = unify_expr index1 index2 in
          let sub2 = unify_expr iter1 iter2 in
          merge_option_skewed sub1 sub2
      | _ -> None)
  | Str s2 ->
      (match expr1 with
      | Str s1 when String.equal s1 s2 -> Some String.Map.empty
      | _ -> None)
  | Call (name2, args2) ->
      (match expr1 with
      | Call (name1, args1) ->
          let args_list = List.zip args1 args2 in
          (match args_list with
          | List.Or_unequal_lengths.Unequal_lengths -> None
          | List.Or_unequal_lengths.Ok l ->
              let sub_names = unify_expr name1 name2 in
              let sub_args =
                List.fold
                  l
                  ~init:(Some String.Map.empty)
                  ~f:(fun sub_accum (arg1, arg2) ->
                    unify_expr arg1 arg2 |> merge_option_skewed sub_accum)
              in
              merge_option_skewed sub_names sub_args)
      | _ -> None)
  | Name name2 ->
      (match expr1 with
      | Name name1 when String.equal name1 name2 -> Some String.Map.empty
      | _ -> None)

let rec unify_stmt : stmt -> stmt -> substitutions option =
 fun stmt1 stmt2 ->
  match stmt1 with
  | Assign (l1, r1) ->
      (match stmt2 with
      | Assign (l2, r2) when equal_lhs l1 l2 -> unify_expr r1 r2
      | _ -> None)
  | For (index1, iter1, body1) ->
      (match stmt2 with
      | For (index2, iter2, body2) when equal_id index1 index2 ->
          let sub_iter = unify_expr iter1 iter2 in
          let sub_body = unify_block body1 body2 in
          merge_option_skewed sub_iter sub_body
      | _ -> None)
  | Return expr1 ->
      (match stmt2 with
      | Return expr2 -> unify_expr expr1 expr2
      | _ -> None)

and unify_block : block -> block -> substitutions option =
 fun block1 block2 ->
  let block_list = List.zip block1 block2 in
  match block_list with
  | List.Or_unequal_lengths.Unequal_lengths -> None
  | List.Or_unequal_lengths.Ok l ->
      List.fold l ~init:(Some String.Map.empty) ~f:(fun sub (stmt1, stmt2) ->
          unify_stmt stmt1 stmt2 |> merge_option_skewed sub)

let unify_defn : defn -> defn -> substitutions option =
 fun (_, body1) (_, body2) -> unify_block body1 body2

let unify_env : env -> env -> substitutions option =
 fun env1 env2 ->
  if not (Int.equal (String.Map.length env1) (String.Map.length env2))
  then None
  else
    String.Map.fold
      env1
      ~init:(Some String.Map.empty)
      ~f:(fun ~key:name ~data:defn1 sub ->
        match String.Map.find env2 name with
        | None -> None
        | Some defn2 -> unify_defn defn1 defn2 |> merge_option_skewed sub)

let unify_naive : program -> program -> substitutions option =
 fun (env1, block1) (env2, block2) ->
  merge_option_skewed (unify_env env1 env2) (unify_block block1 block2)

module L = struct
  type 'a shape =
    | Prog of 'a * 'a
    | Env of 'a list
    | Block of 'a list
    | Stmt of 'a list
    | Expr of 'a list
    | Defn of 'a list
    | Id of id
    | Lhs of id option * 'a list
  [@@deriving ord, show]

  type t = Mk of t shape [@@unboxed]

  (*let pp_shape
      : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a shape -> unit
    =
   fun f fmt shape -> failwith "TODO"*)

  let children : 'a shape -> 'a list = function
    | Prog (env, block) -> [ env; block ]
    | Env defns -> defns
    | Block block -> block
    | Stmt stmt -> stmt
    | Expr children -> children
    | Defn defn -> defn
    | Id _ -> []
    | Lhs (_, children) -> children

  let map_children : 'a shape -> ('a -> 'b) -> 'b shape =
   fun term f ->
    match term with
    | Prog (env, block) -> Prog (f env, f block)
    | Env defns -> Env (List.map defns ~f)
    | Block block -> Block (List.map block ~f)
    | Stmt stmt -> Stmt (List.map stmt ~f)
    | Expr children -> Expr (List.map children ~f)
    | Defn defn -> Defn (List.map defn ~f)
    | Id id -> Id id
    | Lhs (lhs, children) -> Lhs (lhs, List.map children ~f)

  let of_sexp : Sexplib0.Sexp.t -> 'a =
   fun s ->
    let t_of_id : id -> 'a = fun id -> Mk (Id id) in
    let rec t_of_lhs : lhs -> 'a =
     fun lhs ->
      match lhs with
      | Name n -> Mk (Lhs (Some n, []))
      | Index (l, expr) -> Mk (Lhs (None, [ t_of_lhs l; t_of_expr expr ]))
    and t_of_expr : expr -> 'a =
     fun expr ->
      match expr with
      | Name _ | Num _ | Hole _ | Str _ -> Mk (Expr [])
      | Index (e, i) -> Mk (Expr [ t_of_expr e; t_of_expr i ])
      | Call (f, args) -> Mk (Expr (t_of_expr f :: List.map args ~f:t_of_expr))
    in
    let rec t_of_stmt : stmt -> 'a =
     fun stmt ->
      match stmt with
      | Return e -> Mk (Stmt [ t_of_expr e ])
      | For (index, iter, body) ->
          Mk (Stmt [ t_of_id index; t_of_expr iter; t_of_block body ])
      | Assign (lhs, rhs) -> Mk (Stmt [ t_of_lhs lhs; t_of_expr rhs ])
    and t_of_block : block -> 'a =
     fun block -> Mk (Block (List.map block ~f:t_of_stmt))
    in
    let t_of_defn : defn -> 'a =
     fun (args, body) ->
      Mk (Defn (List.map args ~f:t_of_id @ [ t_of_block body ]))
    in
    let t_of_env : env -> 'a =
     fun env ->
      Mk
        (Env
           (String.Map.to_alist env
           |> List.map ~f:(fun (name, defn) -> t_of_defn defn)))
    in
    let t_of_prog : program -> 'a =
     fun (env, block) -> Mk (Prog (t_of_env env, t_of_block block))
    in
    program_of_sexp s |> t_of_prog

  type op =
    | ProgOp
    | EnvOp
    | BlockOp
    | DefnOp
    | IdOp of id
    | StmtOp
    | ExprOp
    | LhsIndexOp
    | LhsNameOp of string
  [@@deriving eq]

  let op : 'a shape -> op = function
    | Prog _ -> ProgOp
    | Env _ -> EnvOp
    | Block _ -> BlockOp
    | Defn _ -> DefnOp
    | Stmt _ -> StmtOp
    | Expr _ -> ExprOp
    | Id id -> IdOp id
    | Lhs (None, _) -> LhsIndexOp
    | Lhs (Some n, _) -> LhsNameOp n

  let make : op -> 'a list -> 'a shape =
   fun op ls ->
    match[@warning "-8"] (op, ls) with
    | ProgOp, [ env; block ] -> Prog (env, block)
    | EnvOp, defns -> Env defns
    | BlockOp, block -> Block block
    | DefnOp, defn -> Defn defn
    | StmtOp, stmt -> Stmt stmt
    (*| ExprIndexOp, [ expr1; expr2 ] ->
        let (Expr (e1, _)) = expr1 in
        let (Expr (e2, _)) = expr2 in
        Expr (Index (e1, e2), [ expr1; expr2 ])
    | CallOp, children ->
        let (Expr (func, _) :: args) = children in
        Expr
          ( Call (func, List.map args ~f:(function Expr (arg, _) -> arg))
          , children )
    | ExprNameOp n, _ -> Expr (Name n, [])
    | NumOp n, _ -> Expr (Num (int_of_string n), [])
    | StrOp s, _ -> Expr (Str s, [])
    | HoleOp h, _ -> Expr (Hole h, [])*)
    | ExprOp, expr -> Expr expr
    | IdOp id, _ -> Id id
    | LhsIndexOp, [ lhs; expr ] -> Lhs (None, [ lhs; expr ])
    | LhsNameOp n, _ -> Lhs (Some n, [])
end

module A = struct
  type t = unit
  type data = unit [@@deriving show, ord, eq]

  let default = ()
end

module MA
    (_ : GRAPH_API
           with type 'p t = (Ego.Id.t L.shape, A.t, A.data, 'p) egraph
            and type 'a shape := 'a L.shape
            and type analysis := A.t
            and type data := A.data
            and type node := L.t) =
struct
  type 'p t = (Ego.Id.t L.shape, A.t, A.data, 'p) egraph

  let make : ro t -> Ego.Id.t L.shape -> A.data = fun graph term -> ()

  let merge : A.t -> A.data -> A.data -> A.data * (bool * bool) =
   fun () l r -> ((), (false, false))

  let modify : 'a t -> Ego.Id.t -> unit = fun graph cls -> ()
end

module EGraph = Make (L) (A) (MA)
open Odot

[@@@warning "-58"]

let unify_egraph : program -> program -> substitutions option =
 fun (env1, block1) (env2, block2) ->
  let graph = EGraph.init () in
  let p1 = (env1, block1) in
  let _ = EGraph.add_node graph (L.of_sexp (sexp_of_program p1)) in
  let g = EGraph.to_dot graph in
  print_file "out.txt" g;
  failwith "TODO"
