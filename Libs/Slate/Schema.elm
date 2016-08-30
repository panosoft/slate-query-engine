module Slate.Schema exposing (..)

import Dict exposing (..)
import Json.Decode as JD
import Json.Encode as JE
import Json.Helper as Json


type alias EntitySchema =
    { type' : String
    , eventNames : List String
    , properties : List PropertySchema
    }


{-| this type is to handle mutually recursive definition between EntitySchema and PropertySchema
-}
type EntitySchemaReference
    = SchemaReference EntitySchema


type alias PropertySchema =
    { name : String
    , entitySchema : Maybe EntitySchemaReference
    , eventNames : List String
    , owned : Bool
    }


propSchema : PropertySchema
propSchema =
    { name = ""
    , entitySchema = Nothing
    , eventNames = []
    , owned = False
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
