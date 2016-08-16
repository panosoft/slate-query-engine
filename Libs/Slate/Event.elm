module Slate.Event exposing (..)

{-| TODO Incomplete definition for Slate Events
-}


type alias Event =
    { name : String
    , data : EventData
    , metadata : Metadata
    }


{-| TODO Incomplete defition for Slate Data
-}
type alias EventData =
    { id : String
    , value : Maybe String
    , version : Maybe Int
    , propertyId : Maybe String
    , oldPosition : Maybe Int
    , newPosition : Maybe Int
    }


type alias Metadata =
    { command : String
    }
