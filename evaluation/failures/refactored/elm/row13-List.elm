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

-- *** List-foldl, semantic

buildFunction : List Type -> Type -> List Type -> Type
buildFunction args currentType remainingTypes =
    List.foldl
        (\x acc -> acc)
        init
        remainingTypes


        case List.reverse remainingTypes ++ currentType :: args of

    case remainingTypes of
        [] ->
            case List.isEmpty args of
                True ->
                    currentType

                False ->
                    Function (List.reverse args) currentType

        t :: ts ->
            buildFunction (currentType :: args) t ts
