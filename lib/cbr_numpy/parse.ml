open Lang
open Core

let rec pat_of_sexp : Sexp.t -> pat =
 fun sexp ->
  match sexp with
  | Sexp.Atom name -> PName name
  | Sexp.List [ Sexp.Atom "Index"; p; e ] ->
      PIndex (pat_of_sexp p, expr_of_sexp e)
  | Sexp.List [ Sexp.Atom "Number_Hole"; Sexp.Atom hole ] -> PHole (Number, hole)
  | Sexp.List [ Sexp.Atom "Array_Hole"; Sexp.Atom hole ] -> PHole (Array, hole)
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
  | Sexp.List [ Sexp.Atom "Num_Hole"; Sexp.Atom hole ] -> Hole (Number, hole)
  | Sexp.List [ Sexp.Atom "Array_Hole"; Sexp.Atom hole ] -> Hole (Array, hole)
  | _ -> failwith ("Invalid expression: " ^ Sexp.to_string sexp)

let rec stmt_of_sexp : Sexp.t -> stmt =
 fun sexp ->
  match sexp with
  | Sexp.List [ Sexp.Atom "Assign"; left; right ] ->
      Assign (pat_of_sexp left, expr_of_sexp right)
  | Sexp.List [ Sexp.Atom "For"; index; iter; body ] ->
      For (pat_of_sexp index, expr_of_sexp iter, block_of_sexp body)
  | Sexp.List [ Sexp.Atom "Return"; e ] -> Return (expr_of_sexp e)
  | Sexp.List [ Sexp.Atom "If"; cond; body; orelse ] ->
      If (expr_of_sexp cond, block_of_sexp body, block_of_sexp orelse)
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

let rec sexp_of_pat : pat -> Sexp.t =
 fun p ->
  match p with
  | PName name -> Sexp.Atom name
  | PIndex (iter, index) ->
      Sexp.List [ Sexp.Atom "Index"; sexp_of_pat iter; sexp_of_expr index ]
  | PHole (Number, name) ->
      Sexp.List [ Sexp.Atom "Number_Hole"; Sexp.Atom name ]
  | PHole (Array, name) -> Sexp.List [ Sexp.Atom "Array_Hole"; Sexp.Atom name ]

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
  | Hole (Number, hole) -> Sexp.List [ Sexp.Atom "Num_Hole"; Sexp.Atom hole ]
  | Hole (Array, hole) -> Sexp.List [ Sexp.Atom "Array_Hole"; Sexp.Atom hole ]

let rec sexp_of_stmt : stmt -> Sexp.t =
 fun s ->
  match s with
  | Assign (left, right) ->
      Sexp.List [ Sexp.Atom "Assign"; sexp_of_pat left; sexp_of_expr right ]
  | For (index, iter, body) ->
      Sexp.List
        [ Sexp.Atom "For"
        ; sexp_of_pat index
        ; sexp_of_expr iter
        ; sexp_of_block body
        ]
  | Return e -> Sexp.List [ Sexp.Atom "Return"; sexp_of_expr e ]
  | If (cond, body, orelse) ->
      Sexp.List
        [ Sexp.Atom "If"
        ; sexp_of_expr cond
        ; sexp_of_block body
        ; sexp_of_block orelse
        ]

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

let sexp_of_substitutions : substitutions -> Sexp.t =
 fun subs ->
  String.Map.to_alist subs
  |> List.sexp_of_t (fun (hole, e) ->
         Sexp.List [ Sexp.Atom hole; sexp_of_expr e ])

let substitutions_of_sexp : Sexp.t -> substitutions =
 fun sexp ->
  List.t_of_sexp
    (fun sexp ->
      match sexp with
      | Sexp.List [ Sexp.Atom hole; e ] -> (hole, expr_of_sexp e)
      | _ -> failwith "Invalid s-expression")
    sexp
  |> String.Map.of_alist_exn

let str_of_program : program -> string =
 fun p -> sexp_of_program p |> Sexp.to_string

let pp_program : ?channel:Out_channel.t -> program -> unit =
 fun ?(channel = stdout) p ->
  let formatter = Format.formatter_of_out_channel channel in
  sexp_of_program p |> Sexp.pp_hum formatter;
  Format.pp_print_flush formatter ()

let rec py_str_of_sexp : Sexp.t -> string =
 fun sexp ->
  match sexp with
  | Sexp.List [ Sexp.Atom "Call"; Sexp.Atom fn; p1 ] ->
      "np." ^ fn ^ "(" ^ py_str_of_sexp p1 ^ ")"
  | Sexp.List [ Sexp.Atom "Call"; Sexp.Atom "mul"; p1; p2 ] ->
      "np.multiply(" ^ py_str_of_sexp p1 ^ ", " ^ py_str_of_sexp p2 ^ ")"
  | Sexp.List [ Sexp.Atom "Call"; Sexp.Atom fn; p1; p2 ] ->
      "np." ^ fn ^ "(" ^ py_str_of_sexp p1 ^ ", " ^ py_str_of_sexp p2 ^ ")"
  | Sexp.List [ Sexp.Atom "Return"; right ] -> py_str_of_sexp right
  | Sexp.List [ right ] -> py_str_of_sexp right
  | Sexp.Atom a -> a
  | _ -> failwith ("Invalid expression: " ^ Sexp.to_string sexp)

and py_str_of_block : block -> string =
 fun block -> block |> sexp_of_block |> py_str_of_sexp

let py_str_of_program : program -> string =
 fun (env, block) -> py_str_of_block block
