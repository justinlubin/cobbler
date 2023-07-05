hasContraryLiterals : List LogicUS.PL.SyntaxSemantics.FormulaPL -> Bool
hasContraryLiterals ls =
    case ls of
        [] ->
            Basics.False

        x :: xs ->
            (||) (List.member (LogicUS.PL.SyntaxSemantics.fplNegation x) xs) (hasContraryLiterals xs)
