open Core

let time_info () =
  [ ( "ocaml_synthesis_time"
    , `Float (Util.Timing_breakdown.time_taken Util.Timing_breakdown.Synthesis)
    )
  ; ( "ocaml_canonicalization_time"
    , `Float
        (Util.Timing_breakdown.time_taken
           Util.Timing_breakdown.Canonicalization) )
  ; ( "ocaml_unification_time"
    , `Float
        (Util.Timing_breakdown.time_taken Util.Timing_breakdown.Unification) )
  ]

let elm_stdlib_fname : string = "backend/bin/Stdlib.json"

let main_elm : int -> bool -> string -> Yojson.Basic.t =
 fun depth ablation input ->
  let open Cbr_fp in
  let stdlib_sigma, stdlib_gamma, stdlib_env =
    In_channel.with_file elm_stdlib_fname ~f:(fun file ->
        Parse_json.definitions (In_channel.input_all file))
  in
  try
    let name, (orig_typ_binders, orig_typ), orig_rhs =
      Parse_json.variable_definition input
    in
    let fvs =
      Set.to_list
        (Set.diff
           (Exp.free_variables orig_rhs)
           (Set.add (Map.key_set stdlib_gamma) name))
    in
    (* Build env *)
    let recursive_call =
      Exp.build_app (Lang.EVar name) (List.map ~f:(fun fv -> Lang.EVar fv) fvs)
    in
    let new_rhs =
      Exp.build_abs
        fvs
        (Exp.replace_subexp
           ~old_subexp:(Lang.EVar name)
           ~new_subexp:recursive_call
           orig_rhs)
    in
    let env = Map.add_exn stdlib_env ~key:name ~data:new_rhs in
    (* Build typ env *)
    let proto_new_typ_scheme =
      ( orig_typ_binders
      , Typ.build_arr
          (List.map fvs ~f:(fun _ -> Typ.fresh_type_var ()))
          orig_typ )
    in
    let proto_gamma =
      stdlib_gamma |> Map.add_exn ~key:name ~data:proto_new_typ_scheme
    in
    let subst =
      Type_system.check_sub
        stdlib_sigma
        proto_gamma
        new_rhs
        (Typ.instantiate proto_new_typ_scheme)
    in
    let new_typ_scheme =
      Typ.generalize
        (Typ.apply_sub subst (Typ.instantiate proto_new_typ_scheme))
    in
    let gamma = Map.update proto_gamma name ~f:(fun _ -> new_typ_scheme) in
    let () = Type_system.well_typed (stdlib_sigma, gamma, env) in
    let problem =
      Synthesis.problem_of_definitions (stdlib_sigma, gamma, env) name
    in
    match Synthesis.solve ~use_unification:(not ablation) ~depth problem with
    | None -> `Assoc ([ ("status", `String "SynthFail") ] @ time_info ())
    | Some (expansions, wrapped_solution) ->
        (try
           Type_system.check
             stdlib_sigma
             gamma
             wrapped_solution
             (Typ.instantiate new_typ_scheme);
           let params, inside =
             wrapped_solution
             |> Exp.replace_subexp
                  ~old_subexp:recursive_call
                  ~new_subexp:(Lang.EVar name)
             |> Exp.clean
             |> Exp.decompose_abs
           in
           let solution =
             Exp.build_abs (List.drop params (List.length fvs)) inside
           in
           `Assoc
             ([ ("status", `String "Success")
              ; ( "solution"
                , `String
                    (Unparse_elm.definition
                       stdlib_sigma
                       name
                       (orig_typ_binders, orig_typ)
                       solution) )
              ; ("size", `Int expansions)
              ]
             @ time_info ())
         with
        | Type_system.IllTyped e ->
            `Assoc
              [ ("status", `String "OutputIllTyped")
              ; ("reason", `String (Exp.show_single e))
              ])
  with
  | Type_system.IllTyped e ->
      `Assoc
        [ ("status", `String "IllTyped")
        ; ("reason", `String (Exp.show_single e))
        ]
  | Type_system.CannotUnify pairs ->
      `Assoc
        [ ("status", `String "CannotUnify")
        ; ( "reason"
          , `List
              (List.map
                 ~f:(fun (t1, t2) ->
                   `List [ `String (Typ.show t1); `String (Typ.show t2) ])
                 pairs) )
        ]
  | Parse_json.ParseFail s ->
      `Assoc [ ("status", `String "ParseFail"); ("reason", `String s) ]
  | Yojson.Json_error s ->
      `Assoc [ ("status", `String "Yojson.Json_error"); ("reason", `String s) ]
  | Yojson.Basic.Util.Type_error (s, _) ->
      `Assoc
        [ ("status", `String "Yojson.Basic.Util.Type_error")
        ; ("reason", `String s)
        ]
  | Invalid_argument s ->
      `Assoc [ ("status", `String "Invalid_argument"); ("reason", `String s) ]

let main_elm_prettify : string -> Yojson.Basic.t =
 fun input ->
  let open Cbr_fp in
  let stdlib_sigma, stdlib_gamma, stdlib_env =
    In_channel.with_file elm_stdlib_fname ~f:(fun file ->
        Parse_json.definitions (In_channel.input_all file))
  in
  try
    let name, typ, rhs = Parse_json.variable_definition input in
    `String (Unparse_elm.definition stdlib_sigma name typ rhs)
  with
  | Parse_json.ParseFail s ->
      `Assoc [ ("status", `String "ParseFail"); ("reason", `String s) ]
  | Yojson.Json_error s ->
      `Assoc [ ("status", `String "Yojson.Json_error"); ("reason", `String s) ]

let main_python : int -> bool -> string -> Yojson.Basic.t =
 fun depth ablation input ->
  let open Cbr_numpy in
  try
    let target = input |> Parse.program_of_str in
    (* let () =
      failwith
        (target |> Np_synthesis.canonicalize |> snd |> [%show: Lang.block])
    in *)
    match
      Np_synthesis.solve depth ~debug:false ~use_egraphs:(not ablation) target
    with
    | None -> `Assoc ([ ("status", `String "SynthFail") ] @ time_info ())
    | Some (expansions, p) ->
        `Assoc
          ([ ("status", `String "Success")
           ; ("solution", `String (Parse.py_str_of_program p))
           ; ("size", `Int expansions)
           ]
          @ time_info ())
  with
  | Parse.ParseFail s ->
      `Assoc [ ("status", `String "IRParseFail"); ("reason", `String s) ]
  | Np_synthesis.EarlyCutoff s ->
      `Assoc
        [ ("status", `String "SynthFail")
        ; ("reason", `String (sprintf "EarlyCutoff: %s" s))
        ]

let () =
  let input = In_channel.input_all In_channel.stdin in
  let result =
    match Array.get (Sys.get_argv ()) 1 with
    | "elm-prettify" -> main_elm_prettify input
    | lang ->
        let depth =
          match
            Array.get (Sys.get_argv ()) 2
            |> String.chop_prefix ~prefix:"--depth="
          with
          | Some depth_str -> Int.of_string depth_str
          | _ -> failwith "unknown depth argument"
        in
        let () =
          match Array.get (Sys.get_argv ()) 3 with
          | "--timing_breakdown=true" -> Util.Timing_breakdown.enable ()
          | "--timing_breakdown=false" -> ()
          | _ -> failwith "unknown timing breakdown argument"
        in
        let ablation =
          match Array.get (Sys.get_argv ()) 4 with
          | "--ablation=true" -> true
          | "--ablation=false" -> false
          | _ -> failwith "unknown ablation argument"
        in
        (match lang with
        | "elm" -> main_elm depth ablation input
        | "python" -> main_python depth ablation input
        | _ -> failwith (sprintf "unknown language '%s'" lang))
  in
  Yojson.Basic.to_channel Out_channel.stdout result
