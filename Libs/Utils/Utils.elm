module Utils.Utils exposing (..)


isNothing : Maybe a -> Bool
isNothing maybe =
    case maybe of
        Just value ->
            False

        Nothing ->
            True


simpleFilterJust : List (Maybe a) -> List a
simpleFilterJust =
    filterJust identity (\old new -> new)


filterJust : (a -> Maybe b) -> (a -> b -> c) -> List a -> List c
filterJust accessor replacer list =
    case list of
        item :: rest ->
            case accessor item of
                Just value ->
                    replacer item value :: filterJust accessor replacer rest

                Nothing ->
                    filterJust accessor replacer rest

        [] ->
            []


filterErr : List (Result error x) -> List error
filterErr results =
    case results of
        result :: rest ->
            case result of
                Err msg ->
                    msg :: filterErr rest

                Ok _ ->
                    filterErr rest

        [] ->
            []


filterOk : List (Result x value) -> List value
filterOk results =
    case results of
        result :: rest ->
            case result of
                Err _ ->
                    filterOk rest

                Ok value ->
                    value :: filterOk rest

        [] ->
            []
