open Core
open Lib
open Lang

let parsed_typ_env1, parsed_env1 = Common.parse_file "programs/test1.lisp"

let%test_unit "test1 well-typed" =
  try
    Type_system.well_typed Lang.default_datatype_env parsed_typ_env1 parsed_env1
  with
  | Type_system.IllTyped e -> failwith (Exp.show e)
