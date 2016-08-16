module Slate.Utils exposing (..)

import Dict exposing (..)
import Slate.Slate exposing (..)


(//) : Maybe a -> a -> a
(//) =
    flip Maybe.withDefault


dereferenceEntity : Dict String entity -> Maybe EntityReference -> entity -> entity
dereferenceEntity entities ref default =
    Dict.get (ref // "") entities // default


getValidEntity : List ( Bool, String ) -> entity -> Result (List String) entity
getValidEntity errorChecks entity =
    let
        errors =
            List.map snd <|
                List.filter fst
                    errorChecks
    in
        if errors == [] then
            Ok entity
        else
            Err errors
