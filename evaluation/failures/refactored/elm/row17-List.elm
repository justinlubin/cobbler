getContacts : List (Shape WorldCoordinates) -> List (Shape WorldCoordinates) -> List Contact
getContacts shapes1 shapes2 =
    case shapes1 of
        shape1 :: remainingShapes1 ->
            getContactsHelp shape1 remainingShapes1 shapes2 shapes2 []

        [] ->
            []

-- *** List.uncons (auto)

getContacts : List (Shape WorldCoordinates) -> List (Shape WorldCoordinates) -> List Contact
getContacts shapes1 shapes2 =
    shapes1
        |> List.uncons [] (\x -> \y -> getContactsHelp x y shapes2 shapes2 [])
