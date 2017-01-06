module Utils.Utils exposing (..)


(?) : Bool -> ( a, a ) -> a
(?) bool ( t, f ) =
    if bool then
        t
    else
        f


(?=) : Maybe a -> a -> a
(?=) =
    flip Maybe.withDefault


{-| lazy version of ?= operator
-}
(?!=) : Maybe a -> (() -> a) -> a
(?!=) maybe lazy =
    case maybe of
        Just x ->
            x

        Nothing ->
            lazy ()


(|?>) : Maybe a -> (a -> b) -> Maybe b
(|?>) =
    flip Maybe.map


(|??>) : Result a b -> (b -> c) -> Result a c
(|??>) =
    flip Result.map


(??=) : Result err value -> (err -> value) -> value
(??=) result f =
    case result of
        Ok value ->
            value

        Err err ->
            f err


firstMap : (a -> b) -> List ( a, c ) -> List ( b, c )
firstMap f =
    List.map (\( x, y ) -> ( f x, y ))


secondMap : (b -> c) -> List ( a, b ) -> List ( a, c )
secondMap f =
    List.map (\( x, y ) -> ( x, f y ))


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
