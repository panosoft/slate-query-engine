port module Test.App exposing (..)

-- import Time exposing (every)

import String exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.App
import Maybe.Extra as MaybeE exposing (isNothing)
import Slate.TestEntities.PersonEntity as PersonEntity exposing (EntirePerson, EntirePersonDict, defaultEntirePerson)
import Slate.TestEntities.AddressEntity as AddressEntity exposing (EntireAddress, EntireAddressDict, defaultEntireAddress)
import Slate.TestEntities.PersonSchema as PersonSchema exposing (..)
import Slate.TestEntities.AddressSchema as AddressSchema exposing (..)
import Utils.Ops exposing (..)
import Slate.Engine.Query exposing (..)
import Slate.Engine.Engine as Engine exposing (..)
import Slate.Common.Event exposing (..)
import Slate.Common.Reference exposing (..)
import Slate.Common.Projection exposing (..)
import Slate.Common.Mutation exposing (..)
import Slate.Common.EntityUtils exposing (..)
import Date exposing (Date)
import Postgres exposing (..)
import DebugF exposing (..)
import ParentChildUpdate exposing (..)


port node : Float -> Cmd msg


type WrappedModel
    = WrappedModel Model


type alias PersonDict =
    Dict EntityReference Person


type alias AddressDict =
    Dict EntityReference Address


type alias Model =
    { entirePersons : EntirePersonDict
    , entireAddresses : EntireAddressDict
    , persons : PersonDict
    , addresses : AddressDict
    , engineModel : Engine.Model Msg
    , queries : Dict Int (WrappedModel -> Result (List String) WrappedModel)
    , didRefresh : Bool
    , listenConnectionId : Maybe Int
    }


type alias Person =
    { name : PersonEntity.Name
    , address : Address
    }


type alias Address =
    { street : String
    }


type alias Entities =
    { persons : Person
    }


type Msg
    = Nop
      -- | Tick Float
    | SlateEngine Engine.Msg
    | EventError EventRecord ( Int, String )
    | EngineError ( Int, String )
    | EventProcessingComplete Int
    | MutationError String String
    | MissingMutationMsg EventRecord
    | MutatePerson EventRecord
    | MutateAddress EventRecord
    | EventProcessingError ( String, String )
    | ConnectError ( Int, String )
    | Connect Int
    | ConnectionLost ( Int, String )
    | ListenUnlistenError String ( Int, String )
    | ListenUnlisten ( Int, String, ListenUnlisten )
    | ListenEvent ( Int, String, String )


engineConfig : Engine.Config Msg
engineConfig =
    { host = "postgresDBServer"
    , port_ = 5432
    , database = "test"
    , user = "charles"
    , password = "testpassword"
    , errorMsg = EngineError
    , eventProcessingErrorMsg = EventProcessingError
    , completionMsg = EventProcessingComplete
    , tagger = SlateEngine
    }


initModel : Model
initModel =
    { entirePersons = Dict.empty
    , entireAddresses = Dict.empty
    , persons = Dict.empty
    , addresses = Dict.empty
    , engineModel = Engine.initModel
    , queries = Dict.empty
    , didRefresh = False
    , listenConnectionId = Nothing
    }


executeQuery : Engine.Model Msg -> Maybe String -> Query Msg -> List String -> Result (List String) ( Engine.Model Msg, Cmd Msg, Int )
executeQuery =
    Engine.executeQuery engineConfig


refreshQuery : Engine.Model Msg -> Int -> ( Engine.Model Msg, Cmd Msg )
refreshQuery =
    Engine.refreshQuery engineConfig


init : ( Model, Cmd Msg )
init =
    let
        result =
            executeQuery initModel.engineModel Nothing personQuery [ "123", "456" ]

        -- executeQuery (Just "id NOT IN (3, 7)") personQuery [ "123", "456" ]
    in
        case result of
            Ok ( engineModel, cmd, queryId ) ->
                { initModel | engineModel = engineModel, queries = Dict.insert queryId projectPersonQuery initModel.queries }
                    ! [ cmd
                      , Postgres.connect ConnectError Connect ConnectionLost 15000 engineConfig.host engineConfig.port_ engineConfig.database engineConfig.user engineConfig.password
                      ]

            Err err ->
                let
                    l =
                        Debug.log "Init error" err
                in
                    initModel ! []


main : Program Never
main =
    -- N.B. the dummy view returns an empty HTML text node
    --      this is just to make the compiler happy since the worker() call Javascript doesn't use a render
    Html.App.program
        { init = init
        , view = (\_ -> text "")
        , update = update
        , subscriptions = subscriptions
        }


mutationError : String -> Model -> (String -> ( Model, Cmd Msg ))
mutationError type_ model =
    (\err -> update (MutationError type_ err) model)


deleteTaggers : CascadingDeletionTaggers Msg
deleteTaggers =
    Dict.fromList [ ( "Address", MutateAddress ) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateEngine : Engine.Msg -> Model -> ( Model, Cmd Msg )
        updateEngine =
            ParentChildUpdate.updateChildApp (Engine.update engineConfig) update .engineModel SlateEngine (\model engineModel -> { model | engineModel = engineModel })
    in
        case msg of
            Nop ->
                model ! []

            ConnectError ( connectionId, error ) ->
                let
                    l =
                        Debug.log "ConnectError" ( connectionId, error )
                in
                    model ! []

            Connect connectionId ->
                let
                    l =
                        Debug.log "Connect" connectionId
                in
                    { model | listenConnectionId = Just connectionId } ! []

            ConnectionLost ( connectionId, error ) ->
                let
                    l =
                        Debug.log "ConnectionLost" ( connectionId, error )
                in
                    model ! []

            -- Tick time ->
            --     -- outputting to a port is a Cmd msg
            --     ( model, node 1 )
            SlateEngine engineMsg ->
                let
                    l =
                        DebugF.log "engineMsg" engineMsg
                in
                    updateEngine engineMsg model

            -- let
            --     l =
            --         DebugF.log "engineMsg" engineMsg
            --
            --     ( ( engineModel, engineCmd ), appMsgs ) =
            --         Engine.update engineMsg model.engineModel
            --
            --     doUpdate msg ( model, cmds ) =
            --         let
            --             ( newModel, newCmd ) =
            --                 update msg model
            --         in
            --             ( newModel, newCmd :: cmds )
            --
            --     ( newModel, myCmds ) =
            --         List.foldl doUpdate ( { model | engineModel = engineModel }, [] ) appMsgs
            --
            --     myCmd =
            --         myCmds
            --             |> List.filter ((/=) Cmd.none)
            --             |> Cmd.batch
            -- in
            --     newModel ! [ myCmd, Cmd.map SlateEngine engineCmd ]
            MutatePerson eventRecord ->
                let
                    ( mutationResult, maybeDelete ) =
                        PersonEntity.handleMutation model.entirePersons model.entireAddresses eventRecord.event
                in
                    mutationResult
                        |??>
                            (\newDict ->
                                let
                                    newModel =
                                        { model | entirePersons = Debug.log "New Person Dictionary" newDict }

                                    ( finalModel, cmd ) =
                                        maybeDelete
                                            |?> (\delete ->
                                                    buildCascadingDeleteMsg eventRecord deleteTaggers MutationError delete
                                                        |?> (\msg -> update msg newModel)
                                                        ?= (model ! [])
                                                )
                                            ?= (newModel ! [])
                                in
                                    newModel ! [ cmd ]
                            )
                        ??= mutationError "Person" model

            MutateAddress eventRecord ->
                let
                    l =
                        Debug.log "MutateAddress" eventRecord
                in
                    AddressEntity.handleMutation model.entireAddresses eventRecord.event
                        |??> (\newDict -> { model | entireAddresses = Debug.log "New Address Dictionary" newDict } ! [])
                        ??= mutationError "Address" model

            EngineError ( queryId, err ) ->
                let
                    l =
                        Debug.log "EngineError" ( queryId, err )
                in
                    model ! []

            EventProcessingComplete queryId ->
                let
                    l =
                        Debug.log "EventProcessingComplete" ""

                    projectionResult : Result (List String) WrappedModel
                    projectionResult =
                        Dict.get queryId model.queries
                            |?> (\projection -> projection <| WrappedModel model)
                            ?= (Err [ "Unknown query id: " ++ (toString queryId) ])

                    crash =
                        projectionResult |??> identity ??= (Debug.crash << String.join "\n")

                    newModel =
                        projectionResult |??> (\wrappedModel -> unwrapModel wrappedModel) ??= (\_ -> model)

                    ( newEngineModel, cmd ) =
                        if newModel.didRefresh == False then
                            refreshQuery newModel.engineModel queryId
                        else
                            -- let
                            --     json =
                            --         Debug.log "json" <| Engine.exportQueryState newModel.engineModel queryId
                            --
                            --     import_ =
                            --         Engine.importQueryState EngineError EventProcessingError EventProcessingComplete SlateEngine
                            --
                            --     result =
                            --         Debug.log "import" <| import_ personQuery newModel.engineModel json
                            -- in
                            ( newModel.engineModel, Cmd.none )
                in
                    { newModel | didRefresh = True, engineModel = newEngineModel } ! [ cmd ]

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

            ListenUnlistenError channel ( connectionId, error ) ->
                let
                    l =
                        Debug.crash <| "Cannot listen to channel ':  " ++ channel ++ "' error: " ++ error
                in
                    model ! []

            ListenUnlisten ( connectionId, channel, type_ ) ->
                let
                    l =
                        Debug.log "ListenUnlisten" ( connectionId, channel, type_ )
                in
                    model ! []

            ListenEvent ( connectionId, channel, message ) ->
                let
                    l =
                        Debug.log "ListenEvent" ( connectionId, channel, message )
                in
                    model ! []


unwrapModel : WrappedModel -> Model
unwrapModel wrappedModel =
    case wrappedModel of
        WrappedModel model ->
            model


projectPersonQuery : WrappedModel -> Result (List String) WrappedModel
projectPersonQuery wrappedModel =
    let
        model =
            unwrapModel wrappedModel

        addresses =
            projectMap toAddress model.entireAddresses

        persons =
            projectMap (toPerson <| successfulProjections addresses) model.entirePersons

        newModel =
            { model | persons = successfulProjections persons, addresses = successfulProjections addresses }

        allErrors =
            allFailedProjections [ failedProjections addresses, failedProjections persons ]
    in
        (allErrors == []) ? ( Ok <| WrappedModel newModel, Err allErrors )


defaultAddress : Address
defaultAddress =
    { street = defaultEntireAddress.street
    }


{-| convert entire address to address
-}
toAddress : EntireAddress -> Result (List String) Address
toAddress entireAddress =
    getValidEntity
        [ ( isNothing entireAddress.street, "street is missing" )
        ]
        { street = entireAddress.street ?= defaultAddress.street
        }


defaultPerson : Person
defaultPerson =
    { name = defaultEntirePerson.name
    , address = defaultAddress
    }


{-| convert entire person to person
-}
toPerson : AddressDict -> EntirePerson -> Result (List String) Person
toPerson addresses entirePerson =
    let
        maybeAddressRef =
            entirePerson.address

        maybeAddress =
            MaybeE.join <| maybeAddressRef |?> (\ref -> Dict.get ref addresses)

        getPerson : Address -> Result (List String) Person
        getPerson address =
            getValidEntity
                [ ( isNothing entirePerson.name, "name is missing" )
                ]
                { name = entirePerson.name ?= defaultPerson.name
                , address = address
                }
    in
        case isNothing maybeAddressRef of
            True ->
                getPerson defaultAddress

            False ->
                maybeAddress |?> getPerson ?= Err [ "Cannot find address id: " ++ (maybeAddressRef ?= "BUG") ]


query : NodeQuery Msg
query =
    Slate.Engine.Query.query MissingMutationMsg


personQuery : Query Msg
personQuery =
    Node { query | schema = personSchema, properties = Just [ "name" ], msg = MutatePerson }
        [ Leaf { query | schema = addressSchema, properties = Just [ "street" ], msg = MutateAddress } ]


emptyEventData : MutatingEventData
emptyEventData =
    { entityId = ""
    , value = Nothing
    , referenceId = Nothing
    , propertyId = Nothing
    , oldPosition = Nothing
    , newPosition = Nothing
    }


testUpdate : Model
testUpdate =
    let
        eventRecord =
            { id = "1"
            , ts = Date.fromTime 0
            , event =
                { name = "Person created"
                , data = Mutating { emptyEventData | entityId = "person-id" }
                , metadata = { initiatorId = "Testing", command = "Create person" }
                , version = Nothing
                }
            , max = Just "3"
            }

        eventRecord2 =
            { id = "2"
            , ts = Date.fromTime 0
            , event =
                { name = "Person name added"
                , data = Mutating { emptyEventData | entityId = "person-id", value = Just """{"first": "Joe", "middle": "", "last": "Mama"}""" }
                , metadata = { initiatorId = "Testing", command = "Add person name" }
                , version = Nothing
                }
            , max = Just "3"
            }

        eventRecord3 =
            { id = "3"
            , ts = Date.fromTime 0
            , event =
                { name = "Person destroyed"
                , data = Mutating { emptyEventData | entityId = "person-id" }
                , metadata = { initiatorId = "Testing", command = "Destroy person" }
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
    let
        channel =
            "eventsinsert"
    in
        model.listenConnectionId
            |?> (\connectionId -> Postgres.listen (ListenUnlistenError channel) ListenUnlisten ListenEvent (model.listenConnectionId ?= 1) channel)
            ?= Sub.none



-- {-| subscribe to input from JS and the clock ticks every second
-- -}
-- subscriptions : Model -> Sub Msg
-- subscriptions _ =
--     Sub.batch
--         ([ Time.every 10000 Tick
--          ]
--         )
