module Utils.Utils exposing (..)


isNothing : Maybe a -> Bool
isNothing maybe =
    case maybe of
        Just value ->
            False

        Nothing ->
            True


filterJust : List (Maybe a) -> List a
filterJust maybes =
    case maybes of
        maybe :: rest ->
            case maybe of
                Just value ->
                    value :: filterJust rest

                Nothing ->
                    filterJust rest

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
