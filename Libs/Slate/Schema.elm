module Slate.Schema exposing (..)

import Dict exposing (..)


type alias EntitySchema =
    { type' : String
    , eventNames : List String
    , properties : List PropertySchema
    }


type alias PropertySchema =
    { name : String
    , eventNames : List String
    , owned : Bool
    }


eventMap : EntitySchema -> List PropertySchema -> Dict String (Maybe Never)
eventMap entitySchema propertySchema =
    let
        entityEvents =
            entitySchema.eventNames

        propertyEvents =
            List.concat <| List.map .eventNames propertySchema
    in
        Dict.fromList <| List.map (Nothing |> flip (,)) <| List.concat [ entityEvents, propertyEvents ]
