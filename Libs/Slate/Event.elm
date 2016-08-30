module Slate.Event exposing (EventRecord, Event, EventData, eventRecordDecoder)

import Json.Decode as Json exposing (..)
import Json.Decode.Extra exposing (..)
import Json.Helper exposing ((///), (<||))
import Date exposing (Date)


{-| TODO Incomplete definition for Slate Events
-}
type alias EventRecord =
    { id : String
    , ts : Date
    , event : Event
    , max : Maybe String
    }


type alias Event =
    { name : String
    , version : Maybe Int
    , data : EventData
    , metadata : Metadata
    }


{-| TODO Incomplete defition for Slate Data
-}
type alias EventData =
    { entityId : String
    , value : Maybe String
    , referenceId : Maybe String
    , propertyId : Maybe String
    , oldPosition : Maybe Int
    , newPosition : Maybe Int
    }


type alias Metadata =
    { command : String
    }


eventRecordDecoder : Json.Decoder EventRecord
eventRecordDecoder =
    Json.succeed EventRecord
        <|| ("id" := string)
        <|| ("ts" := date)
        <|| ("event" := eventDecoder)
        <|| (maybe ("max" := string))


eventDecoder : Json.Decoder Event
eventDecoder =
    Json.succeed Event
        <|| ("name" := string)
        <|| (maybe ("version" := int))
        <|| ("data" := eventDataDecoder)
        <|| ("metadata" := metadataDecoder)


eventDataDecoder : Json.Decoder EventData
eventDataDecoder =
    Json.succeed EventData
        <|| ("entityId" := string)
        <|| (maybe ("value" := string))
        <|| (maybe ("referenceId" := string))
        <|| (maybe ("propertyId" := string))
        <|| (maybe ("oldPosition" := int))
        <|| (maybe ("newPosition" := int))


metadataDecoder : Json.Decoder Metadata
metadataDecoder =
    Json.succeed Metadata
        <|| ("command" := string)
