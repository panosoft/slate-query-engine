module Slate.Utils exposing (..)


getValidEntity : List ( Bool, String ) -> entity -> Result (List String) entity
getValidEntity errorChecks entity =
    let
        errors =
            errorChecks
                |> List.filter fst
                |> List.map snd
    in
        if errors == [] then
            Ok entity
        else
            Err errors
