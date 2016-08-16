module App exposing (..)

import Dict exposing (..)
import PersonEntity exposing (..)
import Utils.Utils exposing (..)
import Slate.Slate exposing (..)
import Slate.Utils exposing (..)


type alias EntireEntities =
    { persons : Dict String EntirePerson
    }


type alias Person =
    { name : Name
    }


type alias Entities =
    { persons : Person
    }


(//) : Maybe a -> a -> a
(//) =
    flip Maybe.withDefault


entireEntities : EntireEntities
entireEntities =
    { persons = Dict.empty
    }


{-| convert entire person to person
-}
toPerson : Entities -> EntirePerson -> Result (List String) Person
toPerson entities entire =
    getValidEntity
        [ ( isNothing entire.name, "name is missing" )
          -- , ( isNothing entire.age, "age is missing" )
        ]
        { name =
            entire.name // defaultName
            -- , age = entire.age // -1
        }
