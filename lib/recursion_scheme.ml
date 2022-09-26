open Core
open Lang

(* let contains : string -> exp -> bool = fun s e -> failwith "TODO"
let transform : exp -> exp -> exp = fun e1 e2 -> failwith "TODO" *)

(* Assumes no mutual recursion? *)
let extract_list_fold : recursive_name:string -> exp -> exp =
 fun ~recursive_name e -> failwith "TODO"
(* fun ~recursive_name -> function
   | EMatch (scrutinee, branches) ->
       let _, nil_rhs = List.Assoc.find_exn ~equal:String.equal branches "Nil" in
       if contains recursive_name nil_rhs
       then failwith "recursive nil case"
       else (
         let _, cons_rhs =
           List.Assoc.find_exn ~equal:String.equal branches "Cons"
         in
         let rec_var = Util.gensym "rec" in
         (* Exp.build_app (EVar "__list_fold") [ scrutinee; nil_rhs ; build_abs [();()] (transform cons_rhs)] *)
         failwith "TODO: need pairs")
   | _ -> failwith "TODO" *)

(* (* For IntList := Nil Unit | Cons (Int, IntList), turn
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
                       (EVar ctor_arg) ) ) )) )) *)
