insertBefore : (b -> a) -> a -> b -> List b -> List b
insertBefore id targetid insertnode list =
    case list of
        [] ->
            []

        node :: rest ->
            case targetid == id node of
                True ->
                    insertnode :: list

                False ->
                    node :: insertBefore id targetid insertnode rest
