disj : List (TUnit -> Goal a) -> Goal a
disj goals =
    case goals of
        [] ->
            MicroKanren.Kernel.unit

        goal :: tailGoals ->
            MicroKanren.Kernel.disjoin (zzz goal) (disj tailGoals)
