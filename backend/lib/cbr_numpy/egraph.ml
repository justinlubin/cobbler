open Ego.Generic
open Lang
open Core
open Parse

type exprType =
  | ENum
  | EIndex
  | ECall
  | EStr of string
  | EName
  | EHole of hole_type * string
[@@deriving ord, hash]

type stmtType =
  | SFor
  | SAssign
  | SReturn
  | SIf
[@@deriving ord, hash]

module L = struct
  type 'a shape =
    | Prog of 'a
    | Block of int * 'a list
    | Stmt of stmtType * 'a list
    | Expr of exprType * 'a list
    | Id of id
  [@@deriving ord, hash]

  type t = Mk of t shape [@@unboxed]

  let pp_shape
      : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a shape -> unit
    =
   fun f fmt shape ->
    match shape with
    | Prog _ -> Format.fprintf fmt "Program"
    | Block (h, _) -> Format.fprintf fmt "Block %d" h
    | Stmt (s, stmt) ->
        (match (s, stmt) with
        | SFor, [ index; iter; body ] ->
            Format.fprintf fmt "For %a in %a %a" f index f iter f body
        | SAssign, [ pat; rhs ] -> Format.fprintf fmt "%a = %a" f pat f rhs
        | SReturn, [ expr ] -> Format.fprintf fmt "Return %a" f expr
        | SIf, [ cond; t; e ] ->
            Format.fprintf fmt "If %a then %a else %a" f cond f t f e
        | _ -> failwith "Invalid statement when printing e-graph")
    | Expr (e, children) ->
        (match (e, children) with
        | ENum, [ num ] -> Format.fprintf fmt "Num %a" f num
        | EName, [ n ] -> Format.fprintf fmt "Name %a" f n
        | ECall, func :: args ->
            Format.fprintf fmt "%a" f func;
            List.iter args ~f:(function arg -> Format.fprintf fmt "%a" f arg)
        | EStr s, [] -> Format.fprintf fmt "Str %s" s
        | EIndex, [ expr; index ] -> Format.fprintf fmt "%a[%a]" f expr f index
        | EHole (_, h), [] -> Format.fprintf fmt "Hole %s" h
        | _ -> failwith "Invalid expression when printing e-graph")
    | Id id -> Format.fprintf fmt "%s" id

  let children : 'a shape -> 'a list = function
    | Prog block -> [ block ]
    | Block (_, block) -> block
    | Stmt (_, stmt) -> stmt
    | Expr (_, expr) -> expr
    | Id _ -> []

  let map_children : 'a shape -> ('a -> 'b) -> 'b shape =
   fun term f ->
    match term with
    | Prog block -> Prog (f block)
    | Block (h, block) -> Block (h, List.map block ~f)
    | Stmt (sType, stmt) -> Stmt (sType, List.map stmt ~f)
    | Expr (eType, expr) -> Expr (eType, List.map expr ~f)
    | Id id -> Id id

  let of_sexp : Sexplib0.Sexp.t -> 'a =
   fun s ->
    let rec t_of_pat : pat -> 'a =
     fun pat ->
      match pat with
      | PName n -> Mk (Expr (EName, [ Mk (Id n) ]))
      | PIndex (l, expr) -> Mk (Expr (EIndex, [ t_of_pat l; t_of_expr expr ]))
      | PHole (t, h) -> Mk (Expr (EHole (t, h), []))
    and t_of_expr : expr -> 'a =
     fun expr ->
      match expr with
      | Name n -> Mk (Expr (EName, [ Mk (Id n) ]))
      | Num n -> Mk (Expr (ENum, [ Mk (Id (string_of_int n)) ]))
      | Hole (t, h) -> Mk (Expr (EHole (t, h), []))
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
      | If (cond, body, orelse) ->
          Mk
            (Stmt (SIf, [ t_of_expr cond; t_of_block body; t_of_block orelse ]))
    and t_of_block : block -> 'a =
     fun block ->
      let children = List.map block ~f:t_of_stmt in
      let h = Hashtbl.hash children in
      Mk (Block (h, children))
    in
    let t_of_prog : program -> 'a =
     fun (_, block) -> Mk (Prog (t_of_block block))
    in
    program_of_sexp s |> t_of_prog

  let to_sexp : t -> Sexplib0.Sexp.t =
   fun t ->
    let id_of_t : t -> id = function
      | Mk (Id s) -> s
      | _ -> failwith "Invalid id"
    in
    let rec pat_of_t : t -> pat = function
      | Mk (Expr (EIndex, [ pat; index ])) ->
          PIndex (pat_of_t pat, expr_of_t index)
      | Mk (Expr (EIndex, children)) ->
          failwith
            (Printf.sprintf
               "Index expression with %d children"
               (List.length children))
      | Mk (Expr (EName, [ Mk (Id n) ])) -> PName n
      | Mk (Expr (EHole (h, t), _)) -> PHole (h, t)
      | _ -> failwith "Invalid pattern when converting e-graph to s-expression"
    and expr_of_t : t -> expr = function
      | Mk (Expr (EName, [ Mk (Id n) ])) -> Name n
      | Mk (Expr (EName, children)) ->
          failwith
            (Printf.sprintf
               "Invalid name node: %d children"
               (List.length children))
      | Mk (Expr (ENum, [ n ])) -> Num (id_of_t n |> int_of_string)
      | Mk (Expr (ENum, n)) ->
          failwith ("Invalid num child: " ^ (List.length n |> string_of_int))
      | Mk (Expr (EStr s, _)) -> Str s
      | Mk (Expr (EHole (t, h), _)) -> Hole (t, h)
      | Mk (Expr (ECall, func :: args)) ->
          Call (expr_of_t func, List.map args ~f:expr_of_t)
      | Mk (Expr (ECall, children)) -> failwith "Invalid call expression"
      | Mk (Expr (EIndex, [ expr; index ])) ->
          Index (expr_of_t expr, expr_of_t index)
      | Mk (Expr (EIndex, children)) ->
          failwith
            (Printf.sprintf
               "Index expression with %d children"
               (List.length children))
      | Mk (Block _) -> failwith "block passed into expr_of_t"
      | Mk (Prog _) -> failwith "program passed into expr_of_t"
      | Mk (Stmt _) -> failwith "statement passed into expr_of_t"
      | Mk (Id id) -> failwith ("id passed into expr_of_t:" ^ id)
    in
    let rec stmt_of_t : t -> stmt = function
      | Mk (Stmt (SFor, [ index; iter; body ])) ->
          For (pat_of_t index, expr_of_t iter, block_of_t body)
      | Mk (Stmt (SAssign, [ lhs; rhs ])) -> Assign (pat_of_t lhs, expr_of_t rhs)
      | Mk (Stmt (SReturn, [ expr ])) -> Return (expr_of_t expr)
      | Mk (Stmt (SIf, [ c; t; e ])) ->
          If (expr_of_t c, block_of_t t, block_of_t e)
      | _ ->
          failwith "Invalid statement when converting e-graph to s-expression"
    and block_of_t : t -> block = function
      | Mk (Block (_, block)) -> List.map block ~f:stmt_of_t
      | _ -> failwith "Invalid block when converting e-graph to s-expression"
    in
    let prog_of_t : t -> program = function
      | Mk (Prog block) -> (String.Map.empty, block_of_t block)
      | _ -> failwith "Invalid program when converting e-graph to s-expression"
    in
    match t with
    | Mk (Prog _) -> prog_of_t t |> sexp_of_program
    | Mk (Expr _) -> expr_of_t t |> sexp_of_expr
    | _ ->
        failwith
          "Invalid top-level node in e-graph to s-expression conversion: must \
           be expression or program"

  type op =
    | ProgOp
    | BlockOp of int
    | IdOp of id
    | ForOp
    | AssignOp
    | ReturnOp
    | NumOp
    | StrOp of string
    | HoleOp of hole_type * string
    | CallOp of int
    | IndexOp
    | NameOp
    | IfOp
  [@@deriving eq]

  let op : 'a shape -> op = function
    | Prog _ -> ProgOp
    | Block (_, block) -> BlockOp (List.length block)
    | Stmt (SFor, _) -> ForOp
    | Stmt (SAssign, _) -> AssignOp
    | Stmt (SReturn, _) -> ReturnOp
    | Stmt (SIf, _) -> IfOp
    | Expr (ENum, _) -> NumOp
    | Expr (EName, _) -> NameOp
    | Expr (EHole (t, h), _) -> HoleOp (t, h)
    | Expr (EStr s, _) -> StrOp s
    | Expr (EIndex, _) -> IndexOp
    | Expr (ECall, args) -> CallOp (List.length args)
    | Id id -> IdOp id

  let make : op -> 'a list -> 'a shape =
   fun op ls ->
    match (op, ls) with
    | ProgOp, [ block ] -> Prog block
    | ProgOp, children ->
        failwith
          (Printf.sprintf "Invalid program: %d children" (List.length children))
    | BlockOp _, block ->
        let h = Hashtbl.hash block in
        Block (h, block)
    | ForOp, stmt -> Stmt (SFor, stmt)
    | AssignOp, stmt -> Stmt (SAssign, stmt)
    | ReturnOp, stmt -> Stmt (SReturn, stmt)
    | IndexOp, expr -> Expr (EIndex, expr)
    | CallOp _, expr -> Expr (ECall, expr)
    | NameOp, n -> Expr (EName, n)
    | NumOp, n -> Expr (ENum, n)
    | StrOp s, _ -> Expr (EStr s, [])
    | HoleOp (t, h), _ -> Expr (EHole (t, h), [])
    | IdOp id, _ -> Id id
    | IfOp, stmt -> Stmt (SIf, stmt)

  let op_of_string : string -> op =
   fun s ->
    match s with
    | "Prog" -> ProgOp
    | "For" -> ForOp
    | "Assign" -> AssignOp
    | "Return" -> ReturnOp
    | "Index" -> IndexOp
    | "If" -> IfOp
    | "Num" -> NumOp
    | "Name" -> NameOp
    | s when String.is_prefix ~prefix:"Block" s ->
        BlockOp (String.chop_prefix_exn s ~prefix:"Block" |> int_of_string)
    | s when String.is_prefix ~prefix:"Call" s ->
        CallOp (String.chop_prefix_exn s ~prefix:"Call" |> int_of_string)
    | s when String.is_prefix ~prefix:"Hole_" s ->
        let hole_type, name =
          match String.split ~on:'_' s with
          | [ _; hole_type; name ] -> (hole_type, name)
          | _ -> failwith ("Invalid hole op:" ^ s)
        in
        let t =
          match hole_type with
          | "List" -> List
          | "Number" -> Number
          | "Arr" -> Array
          | "String" -> String
          | "Constant" -> Constant
          | _ -> failwith ("Invalid hole type: " ^ hole_type)
        in
        HoleOp (t, name)
    | s when String.is_prefix ~prefix:"Str_" s ->
        StrOp (String.chop_prefix_exn s ~prefix:"Str_")
    | _ -> IdOp s

  let string_of_op : op -> string =
   fun op ->
    match op with
    | ProgOp -> "Prog"
    | BlockOp n -> "Block" ^ string_of_int n
    | ForOp -> "For"
    | AssignOp -> "Assign"
    | ReturnOp -> "Return"
    | IfOp -> "If"
    | IndexOp -> "Index"
    | CallOp n -> "Call" ^ string_of_int n
    | NameOp -> "Name"
    | NumOp -> "Num"
    | StrOp s -> "Str_" ^ s
    | HoleOp (t, h) ->
        Printf.sprintf
          "Hole_%s_%s"
          (match t with
          | List -> "List"
          | Number -> "Number"
          | Constant -> "Constant"
          | String -> "String"
          | Array -> "Arr")
          h
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
