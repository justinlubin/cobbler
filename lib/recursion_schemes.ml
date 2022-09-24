open Core
open Lang

(* For IntList := Nil Unit | Cons (Int, IntList), turn
     `Unit` into `forall b. unit -> b`
   and
     `(Int, IntList)` into `forall b. (Int, b) -> b`  *)
let parameterize_recursion : datatype:string -> typ -> typ =
 fun ~datatype tau -> failwith "TODO"

(* For IntList := Nil () | Cons (Int, IntList), turn
     `()` into `()`
   and
     `(hd, tl)` into `(hd, f tl)` *)
let recurse : datatype:string -> f:string -> typ -> exp -> exp =
 fun ~datatype ~f tau e -> failwith "TODO"

let make_cata : datatype:string -> constructors:(string * typ) list -> exp =
 fun ~datatype ~constructors ->
  let datatype_arg = Util.gensym "cata_datatype_arg" in
  let fun_prefix = Util.gensym "cata_f" in
  let ctor_arg = Util.gensym "cata_ctor_arg" in
  Exp.build_abs
    ((datatype_arg, TDatatype datatype)
    :: List.map constructors ~f:(fun (tag, arg_type) ->
           (fun_prefix ^ "$" ^ tag, parameterize_recursion ~datatype arg_type))
    )
    (EMatch
       ( EVar datatype_arg
       , List.map constructors ~f:(fun (tag, arg_type) ->
             ( tag
             , ( ctor_arg
               , EApp
                   ( EVar (fun_prefix ^ "$" ^ tag)
                   , recurse
                       ~datatype
                       ~f:(fun_prefix ^ "$" ^ tag)
                       arg_type
                       (EVar ctor_arg) ) ) )) ))
