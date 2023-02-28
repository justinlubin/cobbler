open PyreAst
open Sexplib0

type py_ast = Concrete.Module.t

let parse_py : string -> py_ast =
 fun content ->
  let open Parser in
  with_context (fun context ->
      match Concrete.parse_module ~context content with
      | Result.Error { Error.message; line; column; _ } ->
          let message =
            Format.sprintf
              "Parsing error at line %d, column %d: %s"
              line
              column
              message
          in
          failwith message
      | Result.Ok ast -> ast)

let rec remove_location : Sexp.t -> Sexp.t =
 fun s ->
  match s with
  | Sexp.Atom a -> s
  | Sexp.List l ->
      (match l with
      | s :: Sexp.List l :: rest ->
          let replacement =
            match l with
            | Sexp.Atom "location" :: _ -> []
            | _ -> List.map remove_location l
          in
          Sexp.List
            ([ remove_location s ] @ replacement @ List.map remove_location rest)
      | _ -> Sexp.List (List.map remove_location l))

let str_of_ast : py_ast -> string =
 fun ast -> Concrete.Module.sexp_of_t ast |> Sexp.to_string

let pprint_ast : ?channel:out_channel -> Concrete.Module.t -> unit =
 fun ?(channel = stdout) ast ->
  let formatter = Format.formatter_of_out_channel channel in
  Concrete.Module.sexp_of_t ast |> Sexp.pp_hum formatter;
  Format.pp_print_flush formatter ()
