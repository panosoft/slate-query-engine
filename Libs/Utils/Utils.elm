module Utils.Utils exposing (..)


(///) : Result err value -> (err -> value) -> value
(///) result f =
    case result of
        Ok value ->
            value

        Err err ->
            f err


fstMap : (a -> b) -> List ( a, c ) -> List ( b, c )
fstMap f =
    List.map (\( x, y ) -> ( f x, y ))


sndMap : (b -> c) -> List ( a, b ) -> List ( a, c )
sndMap f =
    List.map (\( x, y ) -> ( x, f y ))


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


isOk : Result error x -> Bool
isOk result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False


isErr : Result error x -> Bool
isErr =
    not << isOk


getErr : Result a x -> a -> a
getErr result default =
    case result of
        Ok _ ->
            default

        Err err ->
            err


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
