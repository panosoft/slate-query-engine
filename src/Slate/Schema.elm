module Slate.Schema exposing (..)


type alias EntitySchema =
    { type_ : String
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


nullSchema : EntitySchema
nullSchema =
    { type_ = ""
    , eventNames = []
    , properties = []
    }


propSchema : PropertySchema
propSchema =
    { name = ""
    , entitySchema = Nothing
    , eventNames = []
    , owned = False
    }
