module Slate.Utils exposing (..)

import Utils.Ops exposing (..)


getValidEntity : List ( Bool, String ) -> entity -> Result (List String) entity
getValidEntity errorChecks entity =
    let
        errors =
            errorChecks
                |> List.filter fst
                |> List.map snd
    in
        (errors == []) ? ( Ok entity, Err errors )
