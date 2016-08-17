module Slate.Schema exposing (..)


type alias EntitySchema =
    { type' : String
    , eventNames : List String
    , properties : List PropertySchema
    }


type alias PropertySchema =
    { type' : String
    , eventNames : List String
    , owned : Bool
    }
