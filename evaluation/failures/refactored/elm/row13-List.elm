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

-- *** TODO