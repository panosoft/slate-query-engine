module AddressEntity exposing (..)

import Dict exposing (..)
import Slate.Schema exposing (..)
import Slate.Event exposing (Event)
import Slate.EventProcessing exposing (..)
import AddressSchema exposing (..)


type alias EntireAddress =
    { street : Maybe String
    , city : Maybe String
    , state : Maybe String
    , zip : Maybe String
    }


{-| Starting point for all subSets of Addresses
    since events are applied one at a time to build the final subSet entity
-}
entireAddressShell : EntireAddress
entireAddressShell =
    { street = Nothing
    , city = Nothing
    , state = Nothing
    , zip = Nothing
    }


eventMap : Dict String (Maybe Never)
eventMap =
    Slate.Schema.eventMap addressSchema addressProperties


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
