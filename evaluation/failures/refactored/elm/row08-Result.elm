publicationResultText : Result Error PublicationResult -> String
publicationResultText result =
    case result of
        Ok _ ->
            "Message sent"

        Err error ->
            httpErrorToString error

-- *** Result.catamorphism (auto)

publicationResultText : Result Error PublicationResult -> String
publicationResultText result =
    result
        |> Result.catamorphism httpErrorToString (\_ -> "Message sent")