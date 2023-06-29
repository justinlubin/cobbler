open Lang
open Core

exception ParseFail of string
exception UnparseFail of string

let rec pat_of_sexp : Sexp.t -> pat =
 fun sexp ->
  match sexp with
  | Sexp.Atom name -> PName name
  | Sexp.List [ Sexp.Atom "Index"; p; e ] ->
      PIndex (pat_of_sexp p, expr_of_sexp e)
  | Sexp.List [ Sexp.Atom "Number_Hole"; Sexp.Atom hole ] -> PHole (Number, hole)
  | Sexp.List [ Sexp.Atom "Array_Hole"; Sexp.Atom hole ] -> PHole (Array, hole)
  | Sexp.List [ Sexp.Atom "String_Hole"; Sexp.Atom hole ] -> PHole (String, hole)
  | Sexp.List [ Sexp.Atom "Constant_Hole"; Sexp.Atom hole ] ->
      PHole (Constant, hole)
  | Sexp.List [ Sexp.Atom "List_Hole"; Sexp.Atom hole ] -> PHole (List, hole)
  | _ -> raise (ParseFail ("Invalid pattern: " ^ Sexp.to_string sexp))

and expr_of_sexp : Sexp.t -> expr =
 fun sexp ->
  match sexp with
  | Sexp.List [ Sexp.Atom "Num"; Sexp.Atom "False" ] -> Num 0
  | Sexp.List [ Sexp.Atom "Num"; Sexp.Atom "True" ] -> Num 1
  | Sexp.List [ Sexp.Atom "Num"; Sexp.Atom n ] ->
      (try Num (int_of_string n) with
      | Failure _ -> raise (ParseFail (sprintf "could not parse number %s" n)))
  | Sexp.List [ Sexp.Atom "Index"; iter; index ] ->
      Index (expr_of_sexp iter, expr_of_sexp index)
  | Sexp.List (Sexp.Atom "Call" :: name :: args) ->
      Call (expr_of_sexp name, List.map args ~f:expr_of_sexp)
  | Sexp.List [ Sexp.Atom "Str"; Sexp.Atom str ] -> Str str
  | Sexp.Atom name -> Name name
  | Sexp.List [ Sexp.Atom "Num_Hole"; Sexp.Atom hole ] -> Hole (Number, hole)
  | Sexp.List [ Sexp.Atom "Array_Hole"; Sexp.Atom hole ] -> Hole (Array, hole)
  | Sexp.List [ Sexp.Atom "String_Hole"; Sexp.Atom hole ] -> Hole (String, hole)
  | Sexp.List [ Sexp.Atom "Constant_Hole"; Sexp.Atom hole ] ->
      Hole (Constant, hole)
  | Sexp.List [ Sexp.Atom "List_Hole"; Sexp.Atom hole ] -> Hole (List, hole)
  | _ -> raise (ParseFail ("Invalid expression: " ^ Sexp.to_string sexp))

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
  | _ -> raise (ParseFail ("Invalid statement: " ^ Sexp.to_string sexp))

and block_of_sexp : Sexp.t -> block =
 fun sexp ->
  match sexp with
  | Sexp.Atom _ -> raise (ParseFail ("Invalid block: " ^ Sexp.to_string sexp))
  | Sexp.List l -> List.map l ~f:stmt_of_sexp

let parse_defn : Sexp.t -> id * defn =
 fun sexp ->
  match sexp with
  | Sexp.List [ Sexp.Atom name; Sexp.List params; body ] ->
      (name, (List.map params ~f:id_of_sexp, block_of_sexp body))
  | _ ->
      raise (ParseFail ("Invalid function definition: " ^ Sexp.to_string sexp))

let env_of_sexp : Sexp.t -> env =
 fun sexp ->
  match sexp with
  | Sexp.Atom _ ->
      raise
        (ParseFail ("Invalid environment s-expression: " ^ Sexp.to_string sexp))
  | Sexp.List l -> List.map l ~f:parse_defn |> String.Map.of_alist_exn

let program_of_sexp : Sexp.t -> program =
 fun sexp ->
  match sexp with
  | Sexp.List [ env_sexp; block_sexp ] ->
      (env_of_sexp env_sexp, block_of_sexp block_sexp)
  | _ -> raise (ParseFail ("Invalid program: " ^ Sexp.to_string sexp))

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
  | PHole (String, name) ->
      Sexp.List [ Sexp.Atom "String_Hole"; Sexp.Atom name ]
  | PHole (List, name) -> Sexp.List [ Sexp.Atom "List_Hole"; Sexp.Atom name ]
  | PHole (Constant, name) ->
      Sexp.List [ Sexp.Atom "Constant_Hole"; Sexp.Atom name ]

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
  | Hole (String, hole) -> Sexp.List [ Sexp.Atom "String_Hole"; Sexp.Atom hole ]
  | Hole (List, hole) -> Sexp.List [ Sexp.Atom "List_Hole"; Sexp.Atom hole ]
  | Hole (Constant, hole) ->
      Sexp.List [ Sexp.Atom "Constant_Hole"; Sexp.Atom hole ]

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
 fun e -> Sexp.List (Map.to_alist e |> List.map ~f:sexp_of_defn)

let sexp_of_program : program -> Sexp.t =
 fun (env, block) -> Sexp.List [ sexp_of_env env; sexp_of_block block ]

let sexp_of_substitutions : substitutions -> Sexp.t =
 fun subs ->
  Map.to_alist subs
  |> List.sexp_of_t (fun (hole, e) ->
         Sexp.List [ Sexp.Atom hole; sexp_of_expr e ])

let substitutions_of_sexp : Sexp.t -> substitutions =
 fun sexp ->
  List.t_of_sexp
    (fun sexp ->
      match sexp with
      | Sexp.List [ Sexp.Atom hole; e ] -> (hole, expr_of_sexp e)
      | _ -> raise (ParseFail "Invalid s-expression"))
    sexp
  |> String.Map.of_alist_exn

let str_of_program : program -> string =
 fun p -> sexp_of_program p |> Sexp.to_string

let pp_program : ?channel:Out_channel.t -> program -> unit =
 fun ?(channel = stdout) p ->
  let formatter = Format.formatter_of_out_channel channel in
  sexp_of_program p |> Sexp.pp_hum formatter;
  Format.pp_print_flush formatter ()

let rec vectorized_member_accesses : Sexp.t -> string list * Sexp.t =
 fun sexp ->
  match sexp with
  | Sexp.List
      [ Sexp.Atom "Call"
      ; Sexp.List
          [ Sexp.Atom "Call"
          ; Sexp.Atom "np.vectorize"
          ; Sexp.Atom "__memberAccess"
          ; Sexp.List [ Sexp.Atom "Str"; Sexp.Atom "{1}" ]
          ]
      ; Sexp.Atom attr
      ; Sexp.List [ Sexp.Atom "Call"; Sexp.Atom "np.array_object"; sub ]
      ] ->
      let remainder, base = vectorized_member_accesses sub in
      (attr :: remainder, base)
  | _ -> ([], sexp)

let rec py_str_of_sexp : Sexp.t -> string =
 fun sexp ->
  match vectorized_member_accesses sexp with
  | (_ :: _ as attrs), sub ->
      Printf.sprintf
        "np.vectorize(lambda x: x%s)(%s)"
        (attrs |> List.rev |> List.map ~f:(fun a -> "." ^ a) |> String.concat)
        (py_str_of_sexp sub)
  | [], _ ->
      (match sexp with
      | Sexp.List [ Sexp.Atom "Call"; Sexp.Atom "__memberAccess"; p1; p2 ] ->
          Printf.sprintf "%s.%s" (py_str_of_sexp p2) (py_str_of_sexp p1)
      | Sexp.List [ Sexp.Atom "Call"; Sexp.Atom "negate"; p1 ] ->
          Printf.sprintf "-%s" (py_str_of_sexp p1)
      | Sexp.List
          [ Sexp.Atom "Call"
          ; Sexp.Atom
              (("+" | "*" | "-" | "/" | "**" | "==" | "!=" | ">" | "%") as op)
          ; p1
          ; p2
          ] ->
          Printf.sprintf "(%s %s %s)" (py_str_of_sexp p1) op (py_str_of_sexp p2)
      | Sexp.List [ Sexp.Atom "Call"; Sexp.Atom "len"; p1 ] ->
          Printf.sprintf "len(%s)" (py_str_of_sexp p1)
      | Sexp.List [ Sexp.Atom "Call"; Sexp.Atom "range"; p1 ] ->
          Printf.sprintf "np.arange(%s)" (py_str_of_sexp p1)
      | Sexp.List [ Sexp.Atom "Call"; Sexp.Atom "np.convolve_valid"; p1; p2 ] ->
          "np.convolve("
          ^ py_str_of_sexp p1
          ^ ", "
          ^ py_str_of_sexp p2
          ^ ",'valid')"
      | Sexp.List
          [ Sexp.Atom "Call"; Sexp.Atom "np.random.randint_size"; p1; p2; p3 ]
        ->
          "np.random.randint("
          ^ py_str_of_sexp p1
          ^ ", "
          ^ py_str_of_sexp p2
          ^ ", size="
          ^ py_str_of_sexp p3
          ^ ")"
      | Sexp.List [ Sexp.Atom "Call"; Sexp.Atom "np.array_object"; p1 ] ->
          "np.array(list(" ^ py_str_of_sexp p1 ^ "), dtype=object)"
      | Sexp.List
          [ Sexp.Atom "Call"
          ; Sexp.Atom "np.vectorize"
          ; p1
          ; Sexp.List [ Sexp.Atom "Str"; Sexp.Atom "{}" ]
          ] -> "np.vectorize(" ^ py_str_of_sexp p1 ^ ")"
      | Sexp.List
          [ Sexp.Atom "Call"
          ; Sexp.Atom "np.vectorize"
          ; p1
          ; Sexp.List [ Sexp.Atom "Str"; Sexp.Atom set_string ]
          ] ->
          "np.vectorize(" ^ py_str_of_sexp p1 ^ ", excluded=" ^ set_string ^ ")"
      | Sexp.List [ Sexp.Atom "Call"; Sexp.Atom "np.tolist"; p1 ] ->
          "list(" ^ py_str_of_sexp p1 ^ ")"
      | Sexp.List [ Sexp.Atom "Call"; Sexp.Atom "np.filter"; p1; p2 ] ->
          "list(" ^ py_str_of_sexp p1 ^ "[" ^ py_str_of_sexp p2 ^ "])"
      | Sexp.List [ Sexp.Atom "Call"; Sexp.Atom "np.sum_string"; p1 ] ->
          "np.sum(np.array(" ^ py_str_of_sexp p1 ^ ", dtype=\"object\"))"
      | Sexp.List [ Sexp.Atom "Call"; Sexp.Atom "np.full"; size; value ] ->
          (match value with
          | Sexp.List [ Sexp.Atom "Num"; Sexp.Atom "0" ] ->
              Printf.sprintf "np.zeros(%s)" (py_str_of_sexp size)
          | Sexp.List [ Sexp.Atom "Num"; Sexp.Atom "1" ] ->
              Printf.sprintf "np.ones(%s)" (py_str_of_sexp size)
          | _ ->
              Printf.sprintf
                "np.full(%s, %s)"
                (py_str_of_sexp size)
                (py_str_of_sexp value))
      | Sexp.List [ Sexp.Atom "Call"; Sexp.Atom "broadcast"; p1 ] ->
          Printf.sprintf "%s" (py_str_of_sexp p1)
      | Sexp.List [ Sexp.Atom "Call"; Sexp.Atom "sliceToEnd"; p1; p2 ] ->
          Printf.sprintf "%s[%s:]" (py_str_of_sexp p1) (py_str_of_sexp p2)
      | Sexp.List [ Sexp.Atom "Call"; Sexp.Atom "sliceUntil"; p1; p2 ] ->
          Printf.sprintf "%s[:%s]" (py_str_of_sexp p1) (py_str_of_sexp p2)
      | Sexp.List (Sexp.Atom "Call" :: fn :: args) ->
          py_str_of_sexp fn
          ^ "("
          ^ (List.map ~f:py_str_of_sexp args |> String.concat ~sep:", ")
          ^ ")"
      | Sexp.List [ Sexp.Atom "Return"; right ] -> py_str_of_sexp right
      | Sexp.List [ right ] -> py_str_of_sexp right
      | Sexp.List [ Sexp.Atom "Num"; Sexp.Atom n ] -> n
      | Sexp.List [ Sexp.Atom "Str"; Sexp.Atom s ] ->
          Yojson.Basic.to_string (`String s)
      | Sexp.List [ Sexp.Atom "Num_Hole"; _ ]
      | Sexp.List [ Sexp.Atom "Array_Hole"; _ ]
      | Sexp.List [ Sexp.Atom "String_Hole"; _ ]
      | Sexp.List [ Sexp.Atom "Constant_Hole"; _ ]
      | Sexp.List [ Sexp.Atom "List_Hole"; _ ] -> "?"
      | Sexp.List [ Sexp.Atom "Index"; p1; p2 ] ->
          Printf.sprintf "%s[%s]" (py_str_of_sexp p1) (py_str_of_sexp p2)
      | Sexp.Atom "__emptyList" -> "[]"
      | Sexp.Atom a -> a
      | _ ->
          raise
            (UnparseFail
               ("Invalid expression to unparse: " ^ Sexp.to_string sexp)))

and py_str_of_block : block -> string =
 fun block -> block |> sexp_of_block |> py_str_of_sexp

let py_str_of_program : program -> string =
 fun (env, block) -> py_str_of_block block
