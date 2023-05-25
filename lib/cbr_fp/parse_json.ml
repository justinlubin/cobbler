open Core
open Lang
module Json = Yojson.Basic
module J = Yojson.Basic.Util

let is_type_var : string -> bool = fun s -> Char.is_lowercase (String.get s 0)

let is_constructor : string -> bool =
 fun s -> Char.is_uppercase (String.get s 0)

let is_datatype : string -> bool = fun s -> Char.is_uppercase (String.get s 0)
let is_variable : string -> bool = fun s -> Char.is_lowercase (String.get s 0)

(* Types *)

let rec typ_of_json : Json.t -> typ =
 fun j ->
  match j |> J.member "tag" |> J.to_string with
  | "TypeReference" ->
      (match j |> J.member "name" |> J.to_string with
      | "Int" -> TBase BTInt
      | "Float" -> TBase BTFloat
      | "String" -> TBase BTString
      | dt ->
          TDatatype
            ( dt
            , j |> J.member "arguments" |> J.to_list |> List.map ~f:typ_of_json
            ))
  | "UnitType" -> TDatatype ("TUnit", [])
  | "TypeVariable" -> TVar (j |> J.member "name" |> J.to_string)
  | "FunctionType" ->
      let domain =
        j |> J.member "argumentTypes" |> J.to_list |> List.map ~f:typ_of_json
      in
      let codomain = j |> J.member "returnType" |> typ_of_json in
      Typ.build_arr domain codomain
  | s -> failwith (sprintf "unknown type tag '%s'" s)

(* Patterns *)

let pat_of_json : Json.t -> string =
 fun j ->
  match j |> J.member "tag" |> J.to_string with
  | "VariableDefinition" -> j |> J.member "name" |> J.to_string
  | "AnythingPattern" -> Util.gensym "wildcard"
  | s -> failwith (sprintf "unknown pattern tag '%s'" s)

(* Expressions *)

let var_of_json : Json.t -> string =
 fun j ->
  match j |> J.member "tag" |> J.to_string with
  | "VariableReference" -> j |> J.member "name" |> J.to_string
  | "ExternalReference" ->
      let m = j |> J.member "module" |> J.to_string in
      let i = j |> J.member "identifier" |> J.to_string in
      sprintf "%s.%s" m i
  | s -> failwith (sprintf "unknown variable tag '%s'" s)

let rec branch_of_json : Json.t -> branch =
 fun j ->
  let pat = j |> J.member "pattern" in
  match pat |> J.member "tag" |> J.to_string with
  | "DataPattern" ->
      let ctor = j |> J.member "constructor" |> var_of_json in
      let params =
        j |> J.member "arguments" |> J.to_list |> List.map ~f:pat_of_json
      in
      let rhs = j |> J.member "body" |> exp_of_json in
      (ctor, (params, rhs))
  | s -> failwith (sprintf "unknown branch pattern tag '%s'" s)

and exp_of_json : Json.t -> exp =
 fun j ->
  match j |> J.member "tag" |> J.to_string with
  | "IntLiteral" -> EBase (BEInt (j |> J.member "value" |> J.to_int))
  | "FloatLiteral" -> EBase (BEFloat (j |> J.member "value" |> J.to_float))
  | "StringLiteral" -> EBase (BEString (j |> J.member "value" |> J.to_string))
  | "UnitLiteral" -> ECtor ("EUnit", [])
  | "VariableReference" | "ExternalReference" -> EVar (var_of_json j)
  | "AnonymousFunction" ->
      let params =
        j |> J.member "parameters" |> J.to_list |> List.map ~f:pat_of_json
      in
      let body = j |> J.member "body" |> exp_of_json in
      Exp.build_abs params body
  | "CaseExpression" ->
      let scrutinee = j |> J.member "subject" |> exp_of_json in
      let branches =
        j |> J.member "branches" |> J.to_list |> List.map ~f:branch_of_json
      in
      EMatch (scrutinee, branches)
  | "FunctionApplication" ->
      let args =
        j |> J.member "arguments" |> J.to_list |> List.map ~f:exp_of_json
      in
      (match j |> J.member "function" |> exp_of_json with
      | EVar c when is_constructor c -> ECtor (c, args)
      | head -> Exp.build_app head args)
  | s -> failwith (sprintf "unknown expression tag '%s'" s)

(* Definitions *)

type definition =
  | CustomType of string * (string list * (string * typ list) list)
  | VariableDefinition of string * typ * exp

let variant_of_json : Json.t -> string * typ list =
 fun j ->
  ( j |> J.member "name" |> J.to_string
  , j |> J.member "parameterTypes" |> J.to_list |> List.map ~f:typ_of_json )

let param_of_json : Json.t -> string * typ =
 fun j ->
  (j |> J.member "pattern" |> pat_of_json, j |> J.member "type" |> typ_of_json)

let definition_of_json : Json.t -> definition =
 fun j ->
  match j |> J.member "tag" |> J.to_string with
  | "CustomType" ->
      let dt = j |> J.member "name" |> J.to_string in
      let params =
        j |> J.member "parameters" |> J.to_list |> List.map ~f:J.to_string
      in
      let variants =
        j |> J.member "variants" |> J.to_list |> List.map ~f:variant_of_json
      in
      CustomType (dt, (params, variants))
  | "Definition" ->
      let name = j |> J.member "name" |> J.to_string in
      let params, domain =
        j
        |> J.member "parameters"
        |> J.to_list
        |> List.map ~f:param_of_json
        |> List.unzip
      in
      let codomain = j |> J.member "returnType" |> typ_of_json in
      let tau = Typ.build_arr domain codomain in
      let rhs = j |> J.member "expression" |> exp_of_json in
      let body = Exp.build_abs params rhs in
      VariableDefinition (name, tau, body)
  | s -> failwith (sprintf "unknown definition tag '%s'" s)

let merge_definitions : definition list -> datatype_env * typ_env * env =
 fun defs ->
  List.fold_left
    defs
    ~init:(String.Map.empty, String.Map.empty, String.Map.empty)
    ~f:(fun (sigma, gamma, env) -> function
    | CustomType (lhs, rhs) ->
        (String.Map.add_exn sigma ~key:lhs ~data:rhs, gamma, env)
    | VariableDefinition (lhs, tau, body) ->
        ( sigma
        , String.Map.add_exn gamma ~key:lhs ~data:([], tau)
        , String.Map.add_exn env ~key:lhs ~data:body ))

let definitions_of_json : Json.t -> datatype_env * typ_env * env =
 fun j ->
  j
  |> J.member "body"
  |> J.to_list
  |> List.map ~f:definition_of_json
  |> merge_definitions

(* Exports *)

let definitions : string -> datatype_env * typ_env * env =
 fun text -> text |> Json.from_string |> definitions_of_json

let exp : string -> exp = fun text -> text |> Json.from_string |> exp_of_json
let typ : string -> typ = fun text -> text |> Json.from_string |> typ_of_json
