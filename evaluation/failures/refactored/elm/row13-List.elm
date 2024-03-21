buildFunction : List Type -> Type -> List Type -> Type
buildFunction args currentType remainingTypes =
    case remainingTypes of
        [] ->
            case List.isEmpty args of
                True ->
                    currentType

                False ->
                    Function (List.reverse args) currentType

        t :: ts ->
            buildFunction (currentType :: args) t ts

-- *** Nontrivial

buildFunction : List Type -> Type -> List Type -> Type
buildFunction args currentType remainingTypes =
  let
      (newArgs, newCurrentType) =
          List.foldl
              (\x (a, ct) -> (ct :: a, x))
              (args, currentType)
              remainingTypes
  in
  case List.isEmpty newArgs of
      True ->
          newCurrentType

      False ->
          Function (List.reverse newArgs) newCurrentType