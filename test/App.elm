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
import Slate.Engine as Engine exposing (..)
import Date exposing (Date)


-- import Postgres.Postgres exposing (..)


type alias Model =
    { persons : Dict String EntirePerson
    , addresses : Dict String EntireAddress
    , engineModel : Engine.Model Msg
    , connectionId : Maybe Int
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
    | SlateEngine Engine.Msg
    | EventError EventRecord ( Int, String )
    | EngineError ( Int, String )
    | EventProcessingComplete
    | MutationError String String
    | MissingMutationMsg EventRecord
    | MutatePerson EventRecord
    | MutateAddress EventRecord
    | EventProcessingError ( String, String )


eventMsgDispatch : List ( AppEventMsg Msg, Dict String (Maybe Never) )
eventMsgDispatch =
    [ ( MutatePerson, PersonEntity.eventMap )
    , ( MutateAddress, AddressEntity.eventMap )
    ]


initModel : Model
initModel =
    { persons = Dict.empty
    , addresses = Dict.empty
    , engineModel = Engine.initModel "postgresDBServer" 5432 "test" "charles" "testpassword"
    , connectionId = Nothing
    }


init : ( Model, Cmd Msg )
init =
    let
        result =
            Engine.executeQuery personQuery [ "123", "456" ] Nothing EngineError EventProcessingError EventProcessingComplete initModel.engineModel SlateEngine
    in
        case result of
            Ok ( engineModel, cmd ) ->
                { initModel | engineModel = engineModel } ! [ cmd ]

            Err err ->
                let
                    l =
                        Debug.log "Init error" err
                in
                    initModel ! []


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
                ( ( engineModel, engineCmd ), appMsgs ) =
                    Engine.update engineMsg model.engineModel

                doUpdate msg ( model, cmd ) =
                    let
                        ( newModel, newCmd ) =
                            update msg model

                        cmdBatch =
                            Cmd.batch [ cmd, newCmd ]
                    in
                        ( newModel, cmdBatch )

                ( newModel, cmd ) =
                    List.foldl doUpdate ( { model | engineModel = engineModel }, Cmd.none ) appMsgs
            in
                newModel ! [ Cmd.map SlateEngine engineCmd ]

        MutatePerson eventRecord ->
            let
                event =
                    eventRecord.event
            in
                case PersonEntity.mutate event (lookupEntity model.persons event PersonEntity.entirePersonShell) model.addresses of
                    Ok maybePerson ->
                        case maybePerson of
                            Just person ->
                                { model | persons = Dict.insert event.data.entityId person model.persons } ! []

                            Nothing ->
                                { model | persons = Dict.remove event.data.entityId model.persons } ! []

                    Err err ->
                        update (MutationError "Person" err) model

        MutateAddress eventRecord ->
            let
                event =
                    eventRecord.event
            in
                case AddressEntity.mutate event (lookupEntity model.addresses event AddressEntity.entireAddressShell) of
                    Ok maybeAddress ->
                        case maybeAddress of
                            Just address ->
                                { model | addresses = Dict.insert event.data.entityId address model.addresses } ! []

                            Nothing ->
                                { model | addresses = Dict.remove event.data.entityId model.addresses } ! []

                    Err err ->
                        update (MutationError "Address" err) model

        EngineError ( queryId, err ) ->
            let
                l =
                    Debug.log "EngineError" ( queryId, err )
            in
                model ! []

        EventProcessingComplete ->
            let
                l =
                    Debug.log "EventProcessingComplete" ""
            in
                model ! []

        EventError eventRecord ( queryId, err ) ->
            let
                l =
                    Debug.crash <| "Event Processing error: " ++ err ++ " for: " ++ (toString eventRecord) ++ " on query: " ++ (toString queryId)
            in
                model ! []

        MutationError entityType err ->
            let
                l =
                    Debug.crash <| "Cannot mutate model for entity: " ++ entityType ++ " (" ++ err ++ ")"
            in
                model ! []

        MissingMutationMsg eventRecord ->
            let
                l =
                    Debug.crash <| "Bad query, missing mutation message for:  " ++ (toString eventRecord)
            in
                model ! []

        EventProcessingError ( eventStr, error ) ->
            let
                l =
                    Debug.crash <| "Event Processing Error: " ++ (toString eventStr) ++ " error: " ++ error
            in
                model ! []


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
    Slate.Query.query MissingMutationMsg


personQuery : Query Msg
personQuery =
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
    { entityId = ""
    , value = Nothing
    , referenceId = Nothing
    , propertyId = Nothing
    , oldPosition = Nothing
    , newPosition = Nothing
    }


testUpdate =
    let
        eventRecord =
            { id = "1"
            , ts = Date.fromTime 0
            , event = { name = "Person created", data = { emptyEventData | entityId = "person-id" }, metadata = { command = "Create person" }, version = Nothing }
            , max = Just "3"
            }

        eventRecord2 =
            { id = "2"
            , ts = Date.fromTime 0
            , event =
                { name = "Person name added"
                , data = { emptyEventData | entityId = "person-id", value = Just """{"first": "Joe", "middle": "", "last": "Mama"}""" }
                , metadata = { command = "Add person name" }
                , version = Nothing
                }
            , max = Just "3"
            }

        eventRecord3 =
            { id = "3"
            , ts = Date.fromTime 0
            , event =
                { name = "Person destroyed"
                , data = { emptyEventData | entityId = "person-id" }
                , metadata = { command = "Destroy person" }
                , version = Nothing
                }
            , max = Just "3"
            }

        sendEvent eventRecord model =
            fst <| update (MutatePerson eventRecord) model
    in
        List.foldl sendEvent initModel [ eventRecord, eventRecord2, eventRecord3 ]



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
