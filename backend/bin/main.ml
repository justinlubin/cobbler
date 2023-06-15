open Core

let main_elm : string -> Yojson.Basic.t =
 fun input ->
  let open Cbr_fp in
  let stdlib_fname = "backend/bin/Stdlib.json" in
  let sigma, gamma, env =
    In_channel.with_file stdlib_fname ~f:(fun file ->
        Parse_json.definitions (In_channel.input_all file))
  in
  try
    let name, typ, var = Parse_json.variable_definition input in
    let gamma = Map.add_exn gamma ~key:name ~data:typ in
    let env = Map.add_exn env ~key:name ~data:var in
    try
      let () = Type_system.well_typed (sigma, gamma, env) in
      let problem = Synthesis.problem_of_definitions (sigma, gamma, env) name in
      match Synthesis.solve ~use_unification:true ~depth:5 problem with
      | None -> `Assoc [ ("status", `String "SynthFail") ]
      | Some e ->
          `Assoc
            [ ("status", `String "Success")
            ; ("solution", `String (Exp.show_single (Exp.clean e)))
            ]
    with
    | Type_system.IllTyped e ->
        `Assoc
          [ ("status", `String "IllTyped")
          ; ("reason", `String (Exp.show_single e))
          ]
  with
  | Parse_json.ParseFail s ->
      `Assoc [ ("status", `String "ParseFail"); ("reason", `String s) ]
  | Yojson.Json_error s ->
      `Assoc [ ("status", `String "Yojson.Json_error"); ("reason", `String s) ]
  | Yojson.Basic.Util.Type_error (s, _) ->
      `Assoc
        [ ("status", `String "Yojson.Basic.Util.Type_error")
        ; ("reason", `String s)
        ]

let main_python : string -> Yojson.Basic.t =
 fun input ->
  let open Cbr_numpy in
  try
    let target = input |> Sexp.of_string |> Parse.program_of_sexp in
    match
      Cbr_numpy.Np_synthesis.solve 4 ~debug:false Lang.Array target true
    with
    | None -> `Assoc [ ("status", `String "SynthFail") ]
    | Some e ->
        `Assoc
          [ ("status", `String "Success")
          ; ("solution", `String (Parse.py_str_of_program e))
          ]
  with
  | Parse.ParseFail s ->
      `Assoc [ ("status", `String "ParseFail"); ("reason", `String s) ]

let () =
  let input = In_channel.input_all In_channel.stdin in
  let result =
    match Array.get (Sys.get_argv ()) 1 with
    | "elm" -> main_elm input
    | "python" -> main_python input
    | lang -> failwith (sprintf "unknown language '%s'" lang)
  in
  Yojson.Basic.to_channel Out_channel.stdout result
