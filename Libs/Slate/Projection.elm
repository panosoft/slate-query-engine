module Slate.Projection exposing (..)

import Dict exposing (Dict)
import Utils.Utils exposing (..)


projectMap : (a -> b) -> (Dict comparable a -> Dict comparable b)
projectMap f =
    Dict.map (\_ value -> f value)


okOnly : a -> Dict comparable (Result x a) -> Dict comparable a
okOnly default dictResult =
    dictResult
        |> Dict.toList
        |> List.filter (\( key, value ) -> isOk value)
        |> Dict.fromList
        |> projectMap (flip (///) (\_ -> default))


projectionErrors : Dict comparable (Result a b) -> List a
projectionErrors =
    filterErr << Dict.values


allProjectionErrors : List (List (List a)) -> List a
allProjectionErrors =
    List.concat << (List.map List.concat)
