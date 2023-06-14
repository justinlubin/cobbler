open Core
open Cbr_fp
open Lang
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location

let show_definitions : datatype_env * typ_env * env -> string =
 fun (sigma, gamma, env) ->
  let sigma_s =
    sigma
    |> Map.to_alist
    |> List.map ~f:(fun (dt, (params, variants)) ->
           sprintf
             "(type (%s%s)%s)"
             dt
             (List.map ~f:(fun p -> " " ^ p) params |> String.concat)
             (List.map
                ~f:(fun (ctor, domain) ->
                  sprintf
                    " (%s%s)"
                    ctor
                    (List.map ~f:(fun d -> " " ^ Typ.show d) domain
                    |> String.concat))
                variants
             |> String.concat))
    |> String.concat ~sep:"\n"
  in
  let gamma_s =
    gamma
    |> Map.to_alist
    |> List.map ~f:(fun (name, (_, tau)) ->
           sprintf "(%s : %s)" name (Typ.show tau))
    |> String.concat ~sep:"\n"
  in
  let env_s =
    env
    |> Map.to_alist
    |> List.map ~f:(fun (name, body) ->
           sprintf
             "(%s = %s)"
             name
             (Exp.show_multi 0 (Exp.alpha_normalize body)))
    |> String.concat ~sep:"\n"
  in
  sprintf "%s\n----\n%s\n----\n%s" sigma_s gamma_s env_s

let%expect_test "Syntax.elm parses correctly" =
  "elm-programs/json/Syntax.elm.json"
  |> Common.parse_file_json
  |> show_definitions
  |> print_endline;
  [%expect
    {|
    (type (Color a) (Red) (Green) (Blue) (Custom a a a))
    ----
    (f : (Int -> Int))
    (float : Float)
    (g : (a -> ((List a) -> (List a))))
    (g2 : (a -> ((List a) -> (List a))))
    (g3 : (a -> ((List a) -> (List a))))
    (int : Int)
    (match : Int)
    (red : (Color a))
    (string1 : String)
    (string2 : String)
    (unit : (TUnit))
    (yellow : (Color Int))
    ----
    (f = (lambda var0
      ((+ var0) 1)))
    (float = 3.5)
    (g = (lambda var0
      (List.map (lambda var1 var0))))
    (g2 = (lambda var0
      (List.map (lambda var1 var0))))
    (g3 = (lambda var0
      (List.map (lambda var1 var0))))
    (int = 3)
    (match = (match (Custom 255 255 0)
      ((Red) ->
        0)
      ((Green) ->
        1)
      ((Blue) ->
        2)
      ((Custom var0 var1 var2) ->
        ((+ ((+ var0) var1)) var2))))
    (red = Red)
    (string1 = "hello")
    (string2 = ((++ string1) "!"))
    (unit = (EUnit))
    (yellow = (Custom 255 255 0)) |}]

let%expect_test "List1.elm parses" =
  "elm-programs/json/List1.elm.json"
  |> Common.parse_file_json
  |> show_definitions
  |> print_endline;
  [%expect
    {|
    (type (B) (Fal) (Tru))
    (type (L a) (Nil) (Cons a (L a)))
    ----
    (filter : ((a -> (B)) -> ((L a) -> (L a))))
    (map : ((a -> b) -> ((L a) -> (L b))))
    (target : ((a -> (B)) -> ((a -> b) -> ((L a) -> (L b)))))
    ----
    (filter = (lambda var0
      (lambda var1
        (match var1
          ((Nil) ->
            Nil)
          ((Cons var2 var3) ->
            (match (var0 var2)
              ((Fal) ->
                ((filter var0) var3))
              ((Tru) ->
                (Cons var2 ((filter var0) var3)))))))))
    (map = (lambda var0
      (lambda var1
        (match var1
          ((Nil) ->
            Nil)
          ((Cons var2 var3) ->
            (Cons (var0 var2) ((map var0) var3)))))))
    (target = (lambda var0
      (lambda var1
        (lambda var2
          (match var2
            ((Nil) ->
              Nil)
            ((Cons var3 var4) ->
              (match (var0 var3)
                ((Fal) ->
                  (((target var0) var1) var4))
                ((Tru) ->
                  (Cons (var1 var3) (((target var0) var1) var4)))))))))) |}]

let%expect_test "Sugar.elm parses" =
  "elm-programs/json/Sugar.elm.json"
  |> Common.parse_file_json
  |> show_definitions
  |> print_endline;
  [%expect
    {|
    ----
    (filter : ((a -> (Bool)) -> ((List a) -> (List a))))
    (map : ((a -> b) -> ((List a) -> (List b))))
    (target : ((a -> (Bool)) -> ((a -> b) -> ((List a) -> (List b)))))
    ----
    (filter = (lambda var0
      (lambda var1
        (match var1
          ((Basics.Nil) ->
            (Basics.Nil))
          ((Basics.Cons var2) ->
            (match (var0 hd)
              ((Basics.True) ->
                ((filter var0) var2))
              ((Basics.False) ->
                (Basics.Cons hd ((filter var0) var2)))))))))
    (map = (lambda var0
      (lambda var1
        (match var1
          ((Basics.Nil) ->
            (Basics.Nil))
          ((Basics.Cons var2) ->
            (Basics.Cons (var0 hd) ((map var0) var2)))))))
    (target = (lambda var0
      (lambda var1
        (lambda var2
          (match var2
            ((Basics.Nil) ->
              (Basics.Nil))
            ((Basics.Cons var3) ->
              (match (var0 hd)
                ((Basics.True) ->
                  (((target var0) var1) var3))
                ((Basics.False) ->
                  (Basics.Cons (var1 hd) (((target var0) var1) var3)))))))))) |}]
