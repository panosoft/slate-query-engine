module Test.App exposing (..)

import Dict exposing (Dict)
import PersonEntity exposing (..)
import PersonSchema exposing (..)
import Utils.Utils exposing (..)
import Slate.Utils exposing (..)
import Slate.Query exposing (..)


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


personQueryTemplate : Result (List String) (List String)
personQueryTemplate =
    Node { query | schema = Just personSchema, properties = Just [ "name" ] }
        []
        |> buildQueryTemplate


test =
    Debug.log "personQueryTemplate" <| personQueryTemplate



-- TODO remove this example
-- exQuery : List String
-- exQuery =
--     Node { query | entity = Just "A", properties = Just [ "i", "status" ] }
--         [ Leaf { query | entity = Just "B", properties = Just [] }
--         , Node { query | entity = Just "C" }
--             [ Leaf
--                 { query | entity = Just "E" }
--             , Leaf
--                 { query | entity = Just "F", properties = Just [ "n", "o" ] }
--             ]
--         , Leaf { query | entity = Just "X" }
--         ]
--         |> buildQuery ""
