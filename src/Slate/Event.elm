module Slate.Event exposing (EventRecord, Event, EventData, eventRecordDecoder)

import Json.Decode as Json exposing (..)
import Json.Decode.Extra exposing (..)
import Utils.Json as JsonH exposing ((///), (<||))
import Date exposing (Date)


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
