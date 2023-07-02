open Core
open Lang
module Json = Yojson.Basic
module J = Yojson.Basic.Util

exception ParseFail of string

let is_type_var : string -> bool = fun s -> Char.is_lowercase (String.get s 0)

let is_constructor : string -> bool =
 fun s ->
  (not (String.is_substring ~substring:"." s))
  && Char.is_uppercase (String.get s 0)

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
  | s -> raise (ParseFail (sprintf "unknown type tag '%s'" s))

(* Variable parsing *)

let pvar_of_json : Json.t -> string =
 fun j ->
  match j |> J.member "tag" |> J.to_string with
  | "VariableDefinition" -> j |> J.member "name" |> J.to_string
  | "AnythingPattern" -> Util.gensym "wildcard"
  | s -> raise (ParseFail (sprintf "unknown variable pattern tag '%s'" s))

let evar_of_json : Json.t -> string =
 fun j ->
  match j |> J.member "tag" |> J.to_string with
  | "VariableReference" ->
      (match j |> J.member "name" |> J.to_string with
      | "++" -> "append____CBR_builtin"
      | "||" -> "or____CBR_inline"
      | "&&" -> "and____CBR_inline"
      | name -> name)
  | "ExternalReference" ->
      let m = j |> J.member "module" |> J.to_string in
      let i = j |> J.member "identifier" |> J.to_string in
      if is_constructor i then i else sprintf "%s.%s" m i
  | s -> raise (ParseFail (sprintf "unknown expression variable tag '%s'" s))

(* Patterns *)

let pctor_of_json : Json.t -> string * string list =
 fun j ->
  match j |> J.member "tag" |> J.to_string with
  | "ExternalReference" -> (evar_of_json j, [])
  | "DataPattern" ->
      ( j |> J.member "constructor" |> evar_of_json
      , j |> J.member "arguments" |> J.to_list |> List.map ~f:pvar_of_json )
  | "ListPattern" ->
      (match j |> J.member "prefix" |> J.to_list with
      | [] -> ("Nil", [])
      | [ hd ] ->
          (match j |> J.member "rest" with
          | `Null -> raise (ParseFail "TODO")
          | rest -> ("Cons", [ pvar_of_json hd; rest |> pvar_of_json ]))
      | _ -> raise (ParseFail (sprintf "nested list patterns unsupported")))
  | s -> raise (ParseFail (sprintf "unknown constructor pattern tag '%s'" s))

(* Expressions *)

let rec branch_of_json : Json.t -> branch =
 fun j ->
  let ctor, params = j |> J.member "pattern" |> pctor_of_json in
  let rhs = j |> J.member "body" |> exp_of_json in
  (ctor, (params, rhs))

and exp_of_json : Json.t -> exp =
 fun j ->
  match j |> J.member "tag" |> J.to_string with
  | "IntLiteral" -> EBase (BEInt (j |> J.member "value" |> J.to_int))
  | "FloatLiteral" -> EBase (BEFloat (j |> J.member "value" |> J.to_float))
  | "StringLiteral" -> EBase (BEString (j |> J.member "value" |> J.to_string))
  | "UnitLiteral" -> ECtor ("EUnit", [])
  | "VariableReference" | "ExternalReference" ->
      let name = evar_of_json j in
      if is_constructor name then ECtor (name, []) else EVar name
  | "AnonymousFunction" ->
      let params =
        j |> J.member "parameters" |> J.to_list |> List.map ~f:pvar_of_json
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
      (match (j |> J.member "function" |> exp_of_json, args) with
      | ECtor (c, []), _ -> ECtor (c, args)
      | EVar "::", _ -> ECtor ("Cons", args)
      | EVar "|>", [ arg; fn ] -> EApp (fn, arg)
      | head, _ -> Exp.build_app head args)
  | "ListLiteral" ->
      j
      |> J.member "terms"
      |> J.to_list
      |> List.fold_right
           ~init:(ECtor ("Nil", []))
           ~f:(fun e acc -> ECtor ("Cons", [ exp_of_json e; acc ]))
  | s -> raise (ParseFail (sprintf "unknown expression tag '%s'" s))

(* Definitions *)

type definition =
  | CustomType of string * (string list * (string * typ list) list)
  | VariableDefinition of string * typ_scheme * exp

let variant_of_json : Json.t -> string * typ list =
 fun j ->
  ( j |> J.member "name" |> J.to_string
  , j |> J.member "parameterTypes" |> J.to_list |> List.map ~f:typ_of_json )

let param_of_json : Json.t -> string * typ =
 fun j ->
  (j |> J.member "pattern" |> pvar_of_json, j |> J.member "type" |> typ_of_json)

let variable_definition_of_json : Json.t -> string * typ_scheme * exp =
 fun j ->
  let name = j |> J.member "name" |> J.to_string in
  let params, domain =
    j
    |> J.member "parameters"
    |> J.to_list
    |> List.map ~f:param_of_json
    |> List.unzip
  in
  match j |> J.member "returnType" |> J.to_option typ_of_json with
  | Some codomain ->
      let tau = Typ.build_arr domain codomain |> Typ.generalize in
      let rhs = j |> J.member "expression" |> exp_of_json in
      let body = Exp.build_abs params rhs in
      (name, tau, body)
  | None ->
      raise
        (ParseFail
           (sprintf
              "missing type declaration for top-level definition '%s'"
              name))

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
      let s, t, e = variable_definition_of_json j in
      VariableDefinition (s, t, e)
  | s -> raise (ParseFail (sprintf "unknown definition tag '%s'" s))

let merge_definitions : definition list -> datatype_env * typ_env * env =
 fun defs ->
  List.fold_left
    defs
    ~init:(String.Map.empty, String.Map.empty, String.Map.empty)
    ~f:(fun (sigma, gamma, env) -> function
    | CustomType (lhs, rhs) -> (Map.add_exn sigma ~key:lhs ~data:rhs, gamma, env)
    | VariableDefinition (lhs, tau, body) ->
        ( sigma
        , Map.add_exn gamma ~key:lhs ~data:tau
        , Map.add_exn env ~key:lhs ~data:body ))

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

let variable_definition : string -> string * typ_scheme * exp =
 fun text -> text |> Json.from_string |> variable_definition_of_json

let exp : string -> exp = fun text -> text |> Json.from_string |> exp_of_json
let typ : string -> typ = fun text -> text |> Json.from_string |> typ_of_json
