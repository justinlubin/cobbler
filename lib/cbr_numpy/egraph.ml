open Ego.Generic
open Lang
open Core
open Parse

module L = struct
  type 'a shape =
    | Prog of 'a * 'a
    | Env of 'a list
    | Block of 'a list
    | Stmt of stmtType * 'a list
    | Expr of exprType * 'a list
    | Defn of string * 'a list
    | Id of id
    | Lhs of lhsType * 'a list
  [@@deriving ord]

  type t = Mk of t shape [@@unboxed]

  let pp_shape
      : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a shape -> unit
    =
   fun f fmt shape ->
    match shape with
    | Prog _ -> Format.fprintf fmt "Program"
    | Env _ -> Format.fprintf fmt "Env"
    | Block _ -> Format.fprintf fmt "Block"
    | Stmt (s, stmt) ->
        (match[@warning "-8"] s with
        | SFor ->
            let [ index; iter; _ ] = stmt in
            Format.fprintf fmt "For %a in %a" f index f iter
        | SAssign ->
            let [ lhs; rhs ] = stmt in
            Format.fprintf fmt "%a = %a" f lhs f rhs
        | SReturn ->
            let [ expr ] = stmt in
            Format.fprintf fmt "Return %a" f expr)
    | Expr (e, children) ->
        (match[@warning "-8"] e with
        | ENum n -> Format.fprintf fmt "%d" n
        | EName n -> Format.fprintf fmt "%s" n
        | ECall ->
            let[@warning "-8"] (func :: args) = children in
            Format.fprintf fmt "%a" f func;
            List.iter args ~f:(function arg -> Format.fprintf fmt "%a" f arg)
        | EStr s -> Format.fprintf fmt "Str %s" s
        | EIndex ->
            let [ expr; index ] = children in
            Format.fprintf fmt "%a[%a]" f expr f index
        | EHole h -> Format.fprintf fmt "Hole %s" h)
    | Lhs (l, children) ->
        (match[@warning "-8"] l with
        | LName n -> Format.fprintf fmt "%s" n
        | LIndex ->
            let [ lhs; index ] = children in
            Format.fprintf fmt "%a[%a]" f lhs f index)
    | Defn (name, children) ->
        Format.fprintf fmt "%s" name;
        List.iter children ~f:(function child ->
            Format.fprintf fmt "%a" f child)
    | Id id -> Format.fprintf fmt "%s" id

  let children : 'a shape -> 'a list = function
    | Prog (env, block) -> [ env; block ]
    | Env defns -> defns
    | Block block -> block
    | Stmt (_, stmt) -> stmt
    | Expr (_, expr) -> expr
    | Defn (_, defn) -> defn
    | Id _ -> []
    | Lhs (_, children) -> children

  let map_children : 'a shape -> ('a -> 'b) -> 'b shape =
   fun term f ->
    match term with
    | Prog (env, block) -> Prog (f env, f block)
    | Env defns -> Env (List.map defns ~f)
    | Block block -> Block (List.map block ~f)
    | Stmt (sType, stmt) -> Stmt (sType, List.map stmt ~f)
    | Expr (eType, expr) -> Expr (eType, List.map expr ~f)
    | Defn (name, defn) -> Defn (name, List.map defn ~f)
    | Id id -> Id id
    | Lhs (lType, lhs) -> Lhs (lType, List.map lhs ~f)

  let of_sexp : Sexplib0.Sexp.t -> 'a =
   fun s ->
    let t_of_id : id -> 'a = fun id -> Mk (Id id) in
    let rec t_of_lhs : lhs -> 'a =
     fun lhs ->
      match lhs with
      | Name n -> Mk (Lhs (LName n, []))
      | Index (l, expr) -> Mk (Lhs (LIndex, [ t_of_lhs l; t_of_expr expr ]))
    and t_of_expr : expr -> 'a =
     fun expr ->
      match expr with
      | Name n -> Mk (Expr (EName n, []))
      | Num n -> Mk (Expr (ENum n, []))
      | Hole h -> Mk (Expr (EHole h, []))
      | Str s -> Mk (Expr (EStr s, []))
      | Index (e, i) -> Mk (Expr (EIndex, [ t_of_expr e; t_of_expr i ]))
      | Call (f, args) ->
          Mk (Expr (ECall, t_of_expr f :: List.map args ~f:t_of_expr))
    in
    let rec t_of_stmt : stmt -> 'a =
     fun stmt ->
      match stmt with
      | Return e -> Mk (Stmt (SReturn, [ t_of_expr e ]))
      | For (index, iter, body) ->
          Mk (Stmt (SFor, [ t_of_id index; t_of_expr iter; t_of_block body ]))
      | Assign (lhs, rhs) ->
          Mk (Stmt (SAssign, [ t_of_lhs lhs; t_of_expr rhs ]))
    and t_of_block : block -> 'a =
     fun block -> Mk (Block (List.map block ~f:t_of_stmt))
    in
    let t_of_defn : string -> defn -> 'a =
     fun name (args, body) ->
      Mk (Defn (name, List.map args ~f:t_of_id @ [ t_of_block body ]))
    in
    let t_of_env : env -> 'a =
     fun env ->
      Mk
        (Env
           (String.Map.to_alist env
           |> List.map ~f:(fun (name, defn) -> t_of_defn name defn)))
    in
    let t_of_prog : program -> 'a =
     fun (env, block) -> Mk (Prog (t_of_env env, t_of_block block))
    in
    program_of_sexp s |> t_of_prog

  let[@warning "-8"] to_sexp : t -> Sexplib0.Sexp.t =
   fun t ->
    let id_of_t : t -> id = function
      | Mk (Id id) -> id
    in
    let rec lhs_of_t : t -> lhs = function
      | Mk (Lhs (LIndex, [ lhs; index ])) ->
          Index (lhs_of_t lhs, expr_of_t index)
      | Mk (Lhs (LName n, _)) -> Name n
    and expr_of_t : t -> expr = function
      | Mk (Expr (EName n, _)) -> Name n
      | Mk (Expr (ENum n, _)) -> Num n
      | Mk (Expr (EStr s, _)) -> Str s
      | Mk (Expr (EHole h, _)) -> Hole h
      | Mk (Expr (ECall, func :: args)) ->
          Call (expr_of_t func, List.map args ~f:expr_of_t)
      | Mk (Expr (EIndex, [ expr; index ])) ->
          Index (expr_of_t expr, expr_of_t index)
    in
    let rec stmt_of_t : t -> stmt = function
      | Mk (Stmt (SFor, [ index; iter; body ])) ->
          For (id_of_t index, expr_of_t iter, block_of_t body)
      | Mk (Stmt (SAssign, [ lhs; rhs ])) -> Assign (lhs_of_t lhs, expr_of_t rhs)
      | Mk (Stmt (SReturn, [ expr ])) -> Return (expr_of_t expr)
    and block_of_t : t -> block = function
      | Mk (Block block) -> List.map block ~f:stmt_of_t
    in
    let defn_of_t : t -> id * defn = function
      | Mk (Defn (name, children)) ->
          let rev_list = List.rev children in
          let body = List.hd_exn rev_list in
          let args = List.tl_exn rev_list |> List.rev in
          (name, (List.map ~f:id_of_t args, block_of_t body))
    in
    let env_of_t : t -> env = function
      | Mk (Env defns) -> List.map defns ~f:defn_of_t |> String.Map.of_alist_exn
    in
    let prog_of_t : t -> program = function
      | Mk (Prog (env, block)) -> (env_of_t env, block_of_t block)
    in
    prog_of_t t |> sexp_of_program

  type op =
    | ProgOp
    | EnvOp
    | BlockOp
    | DefnOp of string
    | IdOp of id
    | ForOp
    | AssignOp
    | ReturnOp
    | NumOp of int
    | StrOp of string
    | HoleOp of string
    | CallOp
    | ExprIndexOp
    | ExprNameOp of string
    | LhsIndexOp
    | LhsNameOp of string
  [@@deriving eq]

  let op : 'a shape -> op = function
    | Prog _ -> ProgOp
    | Env _ -> EnvOp
    | Block _ -> BlockOp
    | Defn (name, _) -> DefnOp name
    | Stmt (SFor, _) -> ForOp
    | Stmt (SAssign, _) -> AssignOp
    | Stmt (SReturn, _) -> ReturnOp
    | Expr (ENum n, _) -> NumOp n
    | Expr (EName n, _) -> ExprNameOp n
    | Expr (EHole h, _) -> HoleOp h
    | Expr (EStr s, _) -> StrOp s
    | Expr (EIndex, _) -> ExprIndexOp
    | Expr (ECall, _) -> CallOp
    | Id id -> IdOp id
    | Lhs (LIndex, _) -> LhsIndexOp
    | Lhs (LName n, _) -> LhsNameOp n

  let make : op -> 'a list -> 'a shape =
   fun op ls ->
    match[@warning "-8"] (op, ls) with
    | ProgOp, [ env; block ] -> Prog (env, block)
    | EnvOp, defns -> Env defns
    | BlockOp, block -> Block block
    | DefnOp name, defn -> Defn (name, defn)
    | ForOp, stmt -> Stmt (SFor, stmt)
    | AssignOp, stmt -> Stmt (SAssign, stmt)
    | ReturnOp, stmt -> Stmt (SReturn, stmt)
    | ExprIndexOp, expr -> Expr (EIndex, expr)
    | CallOp, expr -> Expr (ECall, expr)
    | ExprNameOp n, _ -> Expr (EName n, [])
    | NumOp n, _ -> Expr (ENum n, [])
    | StrOp s, _ -> Expr (EStr s, [])
    | HoleOp h, _ -> Expr (EHole h, [])
    | IdOp id, _ -> Id id
    | LhsIndexOp, [ lhs; expr ] -> Lhs (LIndex, [ lhs; expr ])
    | LhsNameOp n, _ -> Lhs (LName n, [])

  let op_of_string : string -> op =
   fun s ->
    match s with
    | "Prog" -> ProgOp
    | "Env" -> EnvOp
    | "Block" -> BlockOp
    | s when String.is_prefix ~prefix:"Defn " s ->
        let _, name = String.lsplit2_exn s ~on:' ' in
        DefnOp name
    | "For" -> ForOp
    | "Assign" -> AssignOp
    | "Return" -> ReturnOp
    | "Call" -> CallOp
    | s when String.is_prefix ~prefix:"Num" s ->
        NumOp (String.chop_prefix_exn s ~prefix:"Num" |> int_of_string)
    | s when String.is_prefix ~prefix:"EName" s ->
        ExprNameOp (String.chop_prefix_exn s ~prefix:"EName")
    | _ -> failwith "TODO"

  let string_of_op : op -> string =
   fun op ->
    match op with
    | ProgOp -> "Prog"
    | EnvOp -> "Env"
    | BlockOp -> "Block"
    | DefnOp name -> "Defn " ^ name
    | ForOp -> "For"
    | AssignOp -> "Assign"
    | ReturnOp -> "Return"
    | ExprIndexOp -> "Index"
    | CallOp -> "Call"
    | ExprNameOp n -> "EName " ^ n
    | NumOp n -> "Num " ^ string_of_int n
    | StrOp s -> "Str " ^ s
    | HoleOp h -> "Hole" ^ h
    | IdOp id -> id
    | LhsIndexOp -> "Index"
    | LhsNameOp n -> "Name " ^ n
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

let string_of_op = L.string_of_op
let op_of_string = L.op_of_string
let t_of_sexp = L.of_sexp
