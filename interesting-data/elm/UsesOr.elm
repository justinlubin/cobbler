hasContraryLiterals : List LogicUS.PL.SyntaxSemantics.FormulaPL -> Bool
hasContraryLiterals ls =
    case ls of
        [] ->
            Basics.False

        x :: xs ->
            (||) (fplNegation x) (hasContraryLiterals xs)
