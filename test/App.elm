module Test.App exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.App
import PersonEntity exposing (..)
import AddressEntity exposing (..)
import PersonSchema exposing (..)
import AddressSchema exposing (..)
import Utils.Utils exposing (..)
import Slate.Utils exposing (..)
import Slate.Query exposing (..)
import Slate.Reference exposing (..)
import Slate.Event exposing (..)
import Slate.Engine exposing (..)


type alias Model =
    { persons : Dict String EntirePerson
    , addresses : Dict String EntireAddress
    , engineModel : Slate.Engine.Model
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


type Msg
    = Nop
    | SlateEngine Slate.Engine.Msg
    | EventError Event String
    | EventProcessingComplete
    | MutationError String String
    | MutatePerson Event
    | MutateAddress Event


eventMsgDispatch : List ( Event -> Msg, Dict String (Maybe Never) )
eventMsgDispatch =
    [ ( MutatePerson, PersonEntity.eventMap )
    , ( MutateAddress, AddressEntity.eventMap )
    ]


initModel : Model
initModel =
    Debug.log "initModel" <|
        { persons = Dict.empty
        , addresses = Dict.empty
        , engineModel = Slate.Engine.initModel
        }


init : ( Model, Cmd Msg )
init =
    initModel
        ! []


main : Program Never
main =
    -- N.B. the dummy init which returns an empty Model and no Cmd
    -- N.B. the dummy view returns an empty HTML text node
    --      this is just to make the compiler happy since the worker() call Javascript doesn't use a render
    Html.App.program
        { init = init
        , view = (\_ -> text "")
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            model ! []

        SlateEngine engineMsg ->
            let
                ( engineModel, engineCmd, appCmd ) =
                    Slate.Engine.update engineMsg model.engineModel
            in
                { model | engineModel = engineModel } ! [ appCmd, Cmd.map SlateEngine engineCmd ]

        MutatePerson event ->
            case PersonEntity.mutate event (lookupEntity model.persons event PersonEntity.entirePersonShell) model.addresses of
                Ok maybePerson ->
                    case maybePerson of
                        Just person ->
                            { model | persons = Dict.insert event.data.id person model.persons } ! []

                        Nothing ->
                            { model | persons = Dict.remove event.data.id model.persons } ! []

                Err err ->
                    update (MutationError "Person" err) model

        MutateAddress event ->
            case AddressEntity.mutate event (lookupEntity model.addresses event AddressEntity.entireAddressShell) of
                Ok maybeAddress ->
                    case maybeAddress of
                        Just address ->
                            { model | addresses = Dict.insert event.data.id address model.addresses } ! []

                        Nothing ->
                            { model | addresses = Dict.remove event.data.id model.addresses } ! []

                Err err ->
                    update (MutationError "Address" err) model

        EventProcessingComplete ->
            Debug.log "EventProcessingComplete" <| model ! []

        EventError event err ->
            Debug.crash ("Event Processing error: " ++ err ++ " for event:" ++ (toString event)) <| ( model, Cmd.none )

        MutationError entityType err ->
            Debug.crash ("Cannot mutate model for entity: " ++ entityType ++ "(" ++ err ++ ")") <| ( model, Cmd.none )


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


query : NodeQuery Msg
query =
    Slate.Query.query Nop


personQueryTemplate : Query (Event -> Msg)
personQueryTemplate =
    Node { query | schema = personSchema, properties = Just [ "name" ], msg = MutatePerson }
        [ Leaf { query | schema = addressSchema, properties = Just [ "street" ], msg = MutateAddress } ]



-- test =
--     let
--         sql =
--             case personQueryTemplate of
--                 Ok cmds ->
--                     String.join "\n" cmds
--
--                 Err err ->
--                     String.join "\n" err
--     in
--         Debug.log sql "personQueryTemplate"


emptyEventData =
    { id = ""
    , value = Nothing
    , version = Nothing
    , propertyId = Nothing
    , oldPosition = Nothing
    , newPosition = Nothing
    }


testUpdate =
    let
        event =
            { name = "Person created"
            , data = { emptyEventData | id = "person-id" }
            , metadata = { command = "Create person" }
            }

        event2 =
            { name = "Person name added"
            , data = { emptyEventData | id = "person-id", value = Just """{"first": "Joe", "middle": "", "last": "Mama"}""" }
            , metadata = { command = "Add person name" }
            }

        event3 =
            { name = "Person destroyed"
            , data = { emptyEventData | id = "person-id" }
            , metadata = { command = "Destroy person" }
            }

        sendEvent event model =
            Debug.log "new model" <| fst <| update (MutatePerson event) model
    in
        List.foldl sendEvent initModel [ event, event2, event3 ]



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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
