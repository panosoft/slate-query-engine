module AddressEntity
    exposing
        ( EntireAddress
        , DefaultEntireAddress
        , entireAddressShell
        , defaultEntireAddress
        , entireAddressEncode
        , entireAddressDecode
        , handleMutation
        , mutate
        )

import Dict exposing (..)
import Json.Encode as JE exposing (..)
import Json.Decode as JD exposing (..)
import Utils.Json as JsonU exposing ((///), (<||))
import Slate.Event exposing (Event)
import Slate.EventProcessing exposing (..)
import Slate.Reference exposing (..)
import Utils.Ops exposing (..)


type alias EntireAddress =
    { street : Maybe String
    , city : Maybe String
    , state : Maybe String
    , zip : Maybe String
    }


{-| Starting point for all Addresses
    since events are applied one at a time to build the final entity
-}
entireAddressShell : EntireAddress
entireAddressShell =
    { street = Nothing
    , city = Nothing
    , state = Nothing
    , zip = Nothing
    }


type alias DefaultEntireAddress =
    { street : String
    , city : String
    , state : String
    , zip : String
    }


defaultEntireAddress : DefaultEntireAddress
defaultEntireAddress =
    { street = ""
    , city = ""
    , state = ""
    , zip = ""
    }



-- encoding/decoding


entireAddressEncode : EntireAddress -> String
entireAddressEncode address =
    JE.encode 0 <|
        JE.object <|
            (List.filter (\( _, value ) -> value /= JE.null))
                [ ( "street", JsonU.encMaybe JE.string address.street )
                , ( "city", JsonU.encMaybe JE.string address.city )
                , ( "state", JsonU.encMaybe JE.string address.state )
                , ( "zip", JsonU.encMaybe JE.string address.zip )
                ]


entireAddressDecode : String -> Result String EntireAddress
entireAddressDecode json =
    JD.decodeString
        ((JD.succeed EntireAddress)
            <|| ("street" := JD.maybe JD.string)
            <|| ("city" := JD.maybe JD.string)
            <|| ("state" := JD.maybe JD.string)
            <|| ("zip" := JD.maybe JD.string)
        )
        json


handleMutation : Dict String EntireAddress -> Event -> Result String (Dict String EntireAddress)
handleMutation dict event =
    (mutate event (lookupEntity dict event entireAddressShell))
        |??>
            (\maybeAddress ->
                maybeAddress |?> (\address -> Dict.insert event.data.entityId address dict) ?= Dict.remove event.data.entityId dict
            )


{-| Mutate the Address based on an event
-}
mutate : Event -> EntireAddress -> Result String (Maybe EntireAddress)
mutate event entity =
    let
        setStreet value entity =
            { entity | street = value }

        setCity value entity =
            { entity | city = value }

        setState value entity =
            { entity | state = value }

        setZip value entity =
            { entity | zip = value }
    in
        case event.name of
            "Address created" ->
                Ok <| Just entity

            "Address destroyed" ->
                Ok Nothing

            "Address street added" ->
                Result.map Just <| updatePropertyValue getStringValue setStreet event entity

            "Address street removed" ->
                Ok <| Just <| setStreet Nothing entity

            "Address city added" ->
                Result.map Just <| updatePropertyValue getStringValue setCity event entity

            "Address city removed" ->
                Ok <| Just <| setCity Nothing entity

            "Address state added" ->
                Result.map Just <| updatePropertyValue getStringValue setState event entity

            "Address state removed" ->
                Ok <| Just <| setState Nothing entity

            "Address zip added" ->
                Result.map Just <| updatePropertyValue getStringValue setZip event entity

            "Address zip removed" ->
                Ok <| Just <| setZip Nothing entity

            _ ->
                Debug.crash <| "You forgot to implement a handler for event name: " ++ event.name
