module Slate.Projection exposing (..)

import Result.Extra as ResultE exposing (isOk)
import Dict exposing (Dict)
import Utils.Utils exposing (..)


projectMap : (a -> b) -> (Dict comparable a -> Dict comparable b)
projectMap f =
    Dict.map (\_ value -> f value)


okOnly : a -> Dict comparable (Result x a) -> Dict comparable a
okOnly default dictResult =
    dictResult
        |> Dict.toList
        |> List.filter (snd >> isOk)
        |> Dict.fromList
        |> projectMap (flip (??=) (\_ -> default))


projectionErrors : Dict comparable (Result (List x) b) -> List (List x)
projectionErrors =
    filterErr << Dict.values


allProjectionErrors : List (List (List x)) -> List x
allProjectionErrors =
    List.concat << (List.map List.concat)
