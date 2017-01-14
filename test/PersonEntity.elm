module PersonEntity
    exposing
        ( EntirePerson
        , DefaultEntirePerson
        , Name
        , entirePersonShell
        , defaultEntirePerson
        , entirePersonEncode
        , entirePersonDecode
        , handleMutation
        , mutate
        )

import Dict exposing (..)
import Json.Encode as JE exposing (..)
import Json.Decode as JD exposing (..)
import Utils.Json as JsonU exposing ((///), (<||))
import Slate.Event exposing (Event)
import Slate.Reference exposing (..)
import Slate.EventProcessing exposing (..)
import AddressEntity exposing (..)
import Utils.Ops exposing (..)


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


type alias DefaultEntirePerson =
    { name : Name
    , age : Int
    , address : EntityReference
    }


defaultEntirePerson : DefaultEntirePerson
defaultEntirePerson =
    { name = defaultName
    , age = -1
    , address = ""
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


nameEncode : Name -> JE.Value
nameEncode name =
    JE.object
        [ ( "first", JE.string name.first )
        , ( "middle", JE.string name.middle )
        , ( "last", JE.string name.last )
        ]


nameDecoder : JD.Decoder Name
nameDecoder =
    JD.succeed Name
        <|| ("first" := JD.string)
        <|| ("middle" := JD.string)
        <|| ("last" := JD.string)



-- encoding/decoding


entirePersonEncode : EntirePerson -> String
entirePersonEncode person =
    JE.encode 0 <|
        JE.object <|
            (List.filter (\( _, value ) -> value /= JE.null))
                [ ( "name", JsonU.encMaybe nameEncode person.name )
                , ( "age", JsonU.encMaybe JE.int person.age )
                , ( "address", JsonU.encMaybe Slate.Reference.entityReferenceEncode person.address )
                ]


entirePersonDecode : String -> Result String EntirePerson
entirePersonDecode json =
    JD.decodeString
        ((JD.succeed EntirePerson)
            <|| ("name" := JD.maybe nameDecoder)
            <|| ("age" := JD.maybe JD.int)
            <|| ("address" := JD.maybe Slate.Reference.entityReferenceDecoder)
        )
        json


handleMutation : Dict String EntirePerson -> Dict String EntireAddress -> Event -> Result String (Dict String EntirePerson)
handleMutation dict addresses event =
    mutate event (lookupEntity dict event entirePersonShell) addresses
        |??> (\maybePerson -> maybePerson |?> (\person -> Dict.insert event.data.entityId person dict) ?= Dict.remove event.data.entityId dict)


{-| Mutate the Person based on an event
-}
mutate : Event -> EntirePerson -> Dict String EntireAddress -> Result String (Maybe EntirePerson)
mutate event entity addresses =
    let
        decodeName event =
            getConvertedValue (JD.decodeString nameDecoder) event

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
