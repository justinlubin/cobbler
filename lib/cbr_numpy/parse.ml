open Lang
open Core

let rec lhs_of_sexp : Sexp.t -> lhs =
 fun sexp ->
  match sexp with
  | Sexp.Atom name -> Name name
  | Sexp.List [ Sexp.Atom "Index"; p; e ] ->
      Index (lhs_of_sexp p, expr_of_sexp e)
  | _ -> failwith ("Invalid pattern: " ^ Sexp.to_string sexp)

and expr_of_sexp : Sexp.t -> expr =
 fun sexp ->
  match sexp with
  | Sexp.List [ Sexp.Atom "Num"; Sexp.Atom n ] -> Num (int_of_string n)
  | Sexp.List [ Sexp.Atom "Index"; iter; index ] ->
      Index (expr_of_sexp iter, expr_of_sexp index)
  | Sexp.List (Sexp.Atom "Call" :: name :: args) ->
      Call (expr_of_sexp name, List.map args ~f:expr_of_sexp)
  | Sexp.List [ Sexp.Atom "Str"; Sexp.Atom str ] -> Str str
  | Sexp.Atom name -> Name name
  | _ -> failwith ("Invalid expression: " ^ Sexp.to_string sexp)

let rec stmt_of_sexp : Sexp.t -> stmt =
 fun sexp ->
  match sexp with
  | Sexp.List [ Sexp.Atom "Assign"; left; right ] ->
      Assign (lhs_of_sexp left, expr_of_sexp right)
  | Sexp.List [ Sexp.Atom "For"; Sexp.Atom i; iter; body ] ->
      For (i, expr_of_sexp iter, block_of_sexp body)
  | Sexp.List [ Sexp.Atom "Return"; e ] -> Return (expr_of_sexp e)
  | _ -> failwith ("Invalid statement: " ^ Sexp.to_string sexp)

and block_of_sexp : Sexp.t -> block =
 fun sexp ->
  match sexp with
  | Sexp.Atom _ -> failwith ("Invalid block: " ^ Sexp.to_string sexp)
  | Sexp.List l -> List.map l ~f:stmt_of_sexp

let parse_defn : Sexp.t -> id * defn =
 fun sexp ->
  match sexp with
  | Sexp.List [ Sexp.Atom name; Sexp.List params; body ] ->
      (name, (List.map params ~f:id_of_sexp, block_of_sexp body))
  | _ -> failwith ("Invalid function definition: " ^ Sexp.to_string sexp)

let env_of_sexp : Sexp.t -> env =
 fun sexp ->
  match sexp with
  | Sexp.Atom _ ->
      failwith ("Invalid environment s-expression: " ^ Sexp.to_string sexp)
  | Sexp.List l -> List.map l ~f:parse_defn |> String.Map.of_alist_exn

let program_of_sexp : Sexp.t -> program =
 fun sexp ->
  match sexp with
  | Sexp.List [ env_sexp; block_sexp ] ->
      (env_of_sexp env_sexp, block_of_sexp block_sexp)
  | _ -> failwith ("Invalid program: " ^ Sexp.to_string sexp)

let program_of_str : string -> program =
 fun str -> Sexp.of_string str |> program_of_sexp

let rec sexp_of_lhs : lhs -> Sexp.t =
 fun p ->
  match p with
  | Name name -> Sexp.Atom name
  | Index (iter, index) ->
      Sexp.List [ Sexp.Atom "Index"; sexp_of_lhs iter; sexp_of_expr index ]

and sexp_of_expr : expr -> Sexp.t =
 fun e ->
  match e with
  | Num n -> Sexp.List [ Sexp.Atom "Num"; Sexp.Atom (string_of_int n) ]
  | Index (iter, index) ->
      Sexp.List [ Sexp.Atom "Index"; sexp_of_expr iter; sexp_of_expr index ]
  | Call (name, args) ->
      Sexp.List
        ([ Sexp.Atom "Call"; sexp_of_expr name ] @ List.map args ~f:sexp_of_expr)
  | Str str -> Sexp.List [ Sexp.Atom "Str"; Sexp.Atom str ]
  | Name name -> Sexp.Atom name

let rec sexp_of_stmt : stmt -> Sexp.t =
 fun s ->
  match s with
  | Assign (left, right) ->
      Sexp.List [ Sexp.Atom "Assign"; sexp_of_lhs left; sexp_of_expr right ]
  | For (index, iter, body) ->
      Sexp.List
        [ Sexp.Atom "For"
        ; Sexp.Atom index
        ; sexp_of_expr iter
        ; sexp_of_block body
        ]
  | Return e -> Sexp.List [ Sexp.Atom "Return"; sexp_of_expr e ]

and sexp_of_block : block -> Sexp.t =
 fun b -> Sexp.List (List.map b ~f:sexp_of_stmt)

let sexp_of_defn : id * defn -> Sexp.t =
 fun (name, (params, body)) ->
  Sexp.List
    [ Sexp.Atom name
    ; Sexp.List (List.map params ~f:sexp_of_id)
    ; sexp_of_block body
    ]

let sexp_of_env : env -> Sexp.t =
 fun e -> Sexp.List (String.Map.to_alist e |> List.map ~f:sexp_of_defn)

let sexp_of_program : program -> Sexp.t =
 fun (env, block) -> Sexp.List [ sexp_of_env env; sexp_of_block block ]

let str_of_program : program -> string =
 fun p -> sexp_of_program p |> Sexp.to_string

let pprint_program : ?channel:Out_channel.t -> program -> unit =
 fun ?(channel = stdout) p ->
  let formatter = Format.formatter_of_out_channel channel in
  sexp_of_program p |> Sexp.pp_hum formatter;
  Format.pp_print_flush formatter ()
