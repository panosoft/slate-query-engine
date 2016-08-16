module PersonEntity exposing (..)

import Json.Decode as Json exposing ((:=), maybe, string, int, float)
import Json.JsonHelper exposing ((///), (<||))
import Slate.Slate exposing (..)


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



-- Entity


type alias EntirePerson =
    { name : Maybe Name
    , age : Maybe Int
    }


defaultPerson : EntirePerson
defaultPerson =
    { name = defaultName
    , age = -1
    }


{-| Starting point for all subSets of Person
    since events are applied one at a time to build the final subSet entity
-}
entirePersonShell : EntirePerson
entirePersonShell =
    { name = Nothing
    , age = Nothing
    }


{-| Mutate the Person based on an event
-}
mutatePerson : Event -> EntirePerson -> Result String EntirePerson
mutatePerson event entity =
    let
        decodeName event =
            getConvertedValue (Json.decodeString nameDecoder) event

        setName value entity =
            { entity | name = value }

        setAge value entity =
            { entity | age = value }
    in
        case event.name of
            "Person name added" ->
                updatePropertyValue decodeName setName event entity

            "Person age added" ->
                updatePropertyValue getIntValue setAge event entity

            _ ->
                Debug.crash <| "You forgot to implement event: " ++ event.name
