publicationResultText : Result Error PublicationResult -> String
publicationResultText result =
    case result of
        Ok _ ->
            "Message sent"

        Err error ->
            httpErrorToString error
