pointsPlaceInHelp : Transform3d coordinates defines -> List Vec3 -> List Vec3 -> List Vec3
pointsPlaceInHelp transform points result =
    case points of
        point :: remainingPoints ->
            pointsPlaceInHelp transform remainingPoints (pointPlaceIn transform point :: result)

        [] ->
            result
