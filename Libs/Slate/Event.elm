module Slate.Event exposing (..)

import Json.Decode as Json exposing (..)
import Json.JsonHelper exposing ((///), (<||))


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


eventDecoder : Json.Decoder Event
eventDecoder =
    Json.succeed Event
        <|| ("name" := string)
        <|| ("data" := eventDataDecoder)
        <|| ("metadata" := metadataDecoder)


eventDataDecoder : Json.Decoder EventData
eventDataDecoder =
    Json.succeed EventData
        <|| ("id" := string)
        <|| (maybe ("value" := string))
        <|| (maybe ("version" := int))
        <|| (maybe ("propertyId" := string))
        <|| (maybe ("oldPosition" := int))
        <|| (maybe ("newPosition" := int))


metadataDecoder : Json.Decoder Metadata
metadataDecoder =
    Json.succeed Metadata
        <|| ("command" := string)
