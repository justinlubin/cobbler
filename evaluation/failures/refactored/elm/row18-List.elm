incorporateLines : List String -> List (Block b i) -> List (Block b i)
incorporateLines rawLines ast =
    case rawLines of
        [] ->
            ast

        rawLine :: rawLinesTail ->
            incorporateLines rawLinesTail (incorporateLine rawLine ast)

-- *** TODO