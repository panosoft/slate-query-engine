module PersonEntity exposing (..)

import Dict exposing (..)
import Json.Decode as Json exposing ((:=), maybe, string, int, float)
import Json.JsonHelper exposing ((///), (<||))
import Slate.Event exposing (Event)
import Slate.Schema exposing (..)
import Slate.Reference exposing (..)
import Slate.EventProcessing exposing (..)
import PersonSchema exposing (..)
import AddressEntity exposing (..)


-- Entity


type alias EntirePerson =
    { name : Maybe Name
    , age : Maybe Int
    , address : Maybe EntityReference
    }


{-| Starting point for all subSets of Person
    since events are applied one at a time to build the final subSet entity
-}
entirePersonShell : EntirePerson
entirePersonShell =
    { name = Nothing
    , age = Nothing
    , address = Nothing
    }



-- Value Objects


type alias Name =
    { first : String
    , middle : String
    , last : String
    }


defaultName : Name
defaultName =
    { first = ""
    , middle = ""
    , last = ""
    }


nameDecoder : Json.Decoder Name
nameDecoder =
    Json.succeed Name
        <|| ("first" := string)
        <|| ("middle" := string)
        <|| ("last" := string)


eventMap : Dict String (Maybe Never)
eventMap =
    Slate.Schema.eventMap personSchema personProperties


{-| Mutate the Person based on an event
-}
mutate : Event -> EntirePerson -> Dict String EntireAddress -> Result String (Maybe EntirePerson)
mutate event entity addresses =
    let
        decodeName event =
            getConvertedValue (Json.decodeString nameDecoder) event

        setName value entity =
            { entity | name = value }

        setAge value entity =
            { entity | age = value }

        setAddress value entity =
            { entity | address = value }
    in
        case event.name of
            "Person created" ->
                Ok <| Just entity

            "Person destroyed" ->
                Ok Nothing

            "Person name added" ->
                Result.map Just <| updatePropertyValue decodeName setName event entity

            "Person name removed" ->
                Ok <| Just <| setName Nothing entity

            "Person age added" ->
                Result.map Just <| updatePropertyValue getIntValue setAge event entity

            "Person age removed" ->
                Ok <| Just <| setAge Nothing entity

            "Person address added" ->
                Result.map Just <| updatePropertyReference setAddress event entity

            "Person address removed" ->
                Ok <| Just <| setAddress Nothing entity

            _ ->
                Debug.crash <| "You forgot to implement a handler for event name: " ++ event.name
