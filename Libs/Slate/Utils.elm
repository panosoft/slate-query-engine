module Slate.Utils exposing (..)


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
