houseEmpty : List Customer.Customer -> MapObject.MapObject -> Bool
houseEmpty customers house =
    case customers of
        [] ->
            Basics.True

        customer :: otherCustomers ->
            case Customer.livesHere house customer of
                Basics.True ->
                    Basics.False

                Basics.False ->
                    houseEmpty otherCustomers house

