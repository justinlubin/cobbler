open Ego.Generic
open Lang
open Core
open Parse

module L = struct
  type 'a shape =
    | Prog of 'a
    | Block of 'a list
    | Stmt of stmtType * 'a list
    | Expr of exprType * 'a list
    | Id of id
  [@@deriving ord, hash]

  type t = Mk of t shape [@@unboxed] [@@deriving hash]

  let i = ref 0

  let pp_shape
      : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a shape -> unit
    =
   fun f fmt shape ->
    match shape with
    | Prog _ -> Format.fprintf fmt "Program"
    | Block _ ->
        i := !i + 1;
        Format.fprintf fmt "Block"
    | Stmt (s, stmt) ->
        (match[@warning "-8"] s with
        | SFor ->
            let [ index; iter; body ] = stmt in
            Format.fprintf fmt "For %a in %a %a" f index f iter f body
        | SAssign ->
            let [ pat; rhs ] = stmt in
            Format.fprintf fmt "%a = %a" f pat f rhs
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
        | EHole -> Format.fprintf fmt "Hole")
    | Id id -> Format.fprintf fmt "%s" id

  let children : 'a shape -> 'a list = function
    | Prog block -> [ block ]
    | Block block -> block
    | Stmt (_, stmt) -> stmt
    | Expr (_, expr) -> expr
    | Id _ -> []

  let map_children : 'a shape -> ('a -> 'b) -> 'b shape =
   fun term f ->
    match term with
    | Prog block -> Prog (f block)
    | Block block -> Block (List.map block ~f)
    | Stmt (sType, stmt) -> Stmt (sType, List.map stmt ~f)
    | Expr (eType, expr) -> Expr (eType, List.map expr ~f)
    | Id id -> Id id

  let of_sexp : Sexplib0.Sexp.t -> 'a =
   fun s ->
    let t_of_id : id -> 'a = fun id -> Mk (Id id) in
    let rec t_of_pat : pat -> 'a =
     fun pat ->
      match pat with
      | PName n -> Mk (Expr (EName n, []))
      | PIndex (l, expr) -> Mk (Expr (EIndex, [ t_of_pat l; t_of_expr expr ]))
      | PHole _ -> Mk (Expr (EHole, []))
    and t_of_expr : expr -> 'a =
     fun expr ->
      match expr with
      | Name n -> Mk (Expr (EName n, []))
      | Num n -> Mk (Expr (ENum n, []))
      | Hole _ -> Mk (Expr (EHole, []))
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
          Mk (Stmt (SFor, [ t_of_pat index; t_of_expr iter; t_of_block body ]))
      | Assign (lhs, rhs) ->
          Mk (Stmt (SAssign, [ t_of_pat lhs; t_of_expr rhs ]))
    and t_of_block : block -> 'a =
     fun block -> Mk (Block (List.map block ~f:t_of_stmt))
    in
    let t_of_prog : program -> 'a =
     fun (_, block) -> Mk (Prog (t_of_block block))
    in
    program_of_sexp s |> t_of_prog

  let[@warning "-8"] to_sexp : t -> Sexplib0.Sexp.t =
   fun t ->
    let id_of_t : t -> id = function
      | Mk (Id id) -> id
    in
    let rec pat_of_t : t -> pat = function
      | Mk (Expr (EIndex, [ pat; index ])) ->
          PIndex (pat_of_t pat, expr_of_t index)
      | Mk (Expr (EName n, _)) -> PName n
    and expr_of_t : t -> expr = function
      | Mk (Expr (EName n, _)) -> Name n
      | Mk (Expr (ENum n, _)) -> Num n
      | Mk (Expr (EStr s, _)) -> Str s
      | Mk (Expr (EHole, _)) -> failwith "Hole in reference e-graph"
      | Mk (Expr (ECall, func :: args)) ->
          Call (expr_of_t func, List.map args ~f:expr_of_t)
      | Mk (Expr (EIndex, [ expr; index ])) ->
          Index (expr_of_t expr, expr_of_t index)
    in
    let rec stmt_of_t : t -> stmt = function
      | Mk (Stmt (SFor, [ index; iter; body ])) ->
          For (pat_of_t index, expr_of_t iter, block_of_t body)
      | Mk (Stmt (SAssign, [ lhs; rhs ])) -> Assign (pat_of_t lhs, expr_of_t rhs)
      | Mk (Stmt (SReturn, [ expr ])) -> Return (expr_of_t expr)
    and block_of_t : t -> block = function
      | Mk (Block block) -> List.map block ~f:stmt_of_t
    in
    let prog_of_t : t -> program = function
      | Mk (Prog block) -> (String.Map.empty, block_of_t block)
    in
    match t with
    | Mk (Prog _) -> prog_of_t t |> sexp_of_program
    | Mk (Expr _) -> expr_of_t t |> sexp_of_expr
  (*| Mk (Pat _) -> pat_of_t t |> sexp_of_pat*)

  type op =
    | ProgOp
    | BlockOp
    | IdOp of id
    | ForOp
    | AssignOp
    | ReturnOp
    | NumOp of int
    | StrOp of string
    | HoleOp
    | CallOp
    | ExprIndexOp
    | ExprNameOp of string
  [@@deriving eq]

  let op : 'a shape -> op = function
    | Prog _ -> ProgOp
    | Block _ -> BlockOp
    | Stmt (SFor, _) -> ForOp
    | Stmt (SAssign, _) -> AssignOp
    | Stmt (SReturn, _) -> ReturnOp
    | Expr (ENum n, _) -> NumOp n
    | Expr (EName n, _) -> ExprNameOp n
    | Expr (EHole, _) -> HoleOp
    | Expr (EStr s, _) -> StrOp s
    | Expr (EIndex, _) -> ExprIndexOp
    | Expr (ECall, _) -> CallOp
    | Id id -> IdOp id

  let make : op -> 'a list -> 'a shape =
   fun op ls ->
    match[@warning "-8"] (op, ls) with
    | ProgOp, [ block ] -> Prog block
    | BlockOp, block -> Block block
    | ForOp, stmt -> Stmt (SFor, stmt)
    | AssignOp, stmt -> Stmt (SAssign, stmt)
    | ReturnOp, stmt -> Stmt (SReturn, stmt)
    | ExprIndexOp, expr -> Expr (EIndex, expr)
    | CallOp, expr -> Expr (ECall, expr)
    | ExprNameOp n, _ -> Expr (EName n, [])
    | NumOp n, _ -> Expr (ENum n, [])
    | StrOp s, _ -> Expr (EStr s, [])
    | HoleOp, _ -> Expr (EHole, [])
    | IdOp id, _ -> Id id

  let op_of_string : string -> op =
   fun s ->
    match s with
    | "Prog" -> ProgOp
    | "Block" -> BlockOp
    | "For" -> ForOp
    | "Assign" -> AssignOp
    | "Return" -> ReturnOp
    | "Call" -> CallOp
    | "Hole" -> HoleOp
    | "EIndex" | "LIndex" | "ExprIndex" -> ExprIndexOp
    | s when String.is_prefix ~prefix:"Num" s ->
        NumOp (String.chop_prefix_exn s ~prefix:"Num" |> int_of_string)
    | s when String.is_prefix ~prefix:"EName" s ->
        ExprNameOp (String.chop_prefix_exn s ~prefix:"EName")
    | s when String.is_prefix ~prefix:"LName" s ->
        ExprNameOp (String.chop_prefix_exn s ~prefix:"LName")
    | _ -> IdOp s

  let string_of_op : op -> string =
   fun op ->
    match op with
    | ProgOp -> "Prog"
    | BlockOp -> "Block"
    | ForOp -> "For"
    | AssignOp -> "Assign"
    | ReturnOp -> "Return"
    | ExprIndexOp -> "Index"
    | CallOp -> "Call"
    | ExprNameOp n -> "EName " ^ n
    | NumOp n -> "Num " ^ string_of_int n
    | StrOp s -> "Str " ^ s
    | HoleOp -> "Hole"
    | IdOp id -> id
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

module C = struct
  type t = int [@@deriving ord]

  let cost f : Ego.Id.t L.shape -> t =
   fun s ->
    let node_cost =
      match s with
      | L.Prog _ | L.Block _ -> 0
      | L.Stmt _ | L.Expr _ | L.Id _ -> 1
    in
    node_cost + List.fold (L.children s) ~init:0 ~f:(fun sum c -> sum + f c)
end

module Extractor = MakeExtractor (L) (C)

let string_of_op = L.string_of_op
let op_of_string = L.op_of_string
let t_of_sexp = L.of_sexp
let sexp_of_t = L.to_sexp
