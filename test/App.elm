port module Test.App exposing (..)

-- import Time exposing (every)

import String exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.App
import Maybe.Extra as MaybeE exposing (isNothing)
import Result.Extra as ResultE exposing (isErr)
import PersonEntity exposing (EntirePerson, defaultEntirePerson)
import AddressEntity exposing (EntireAddress, defaultEntireAddress)
import PersonSchema exposing (..)
import AddressSchema exposing (..)
import Utils.Utils as Utils exposing (..)
import Slate.Utils exposing (..)
import Slate.Query exposing (..)
import Slate.Event exposing (..)
import Slate.Engine as Engine exposing (..)
import Slate.Projection exposing (..)
import Date exposing (Date)
import Postgres exposing (..)
import DebugF exposing (..)


port node : Float -> Cmd msg


type WrappedModel
    = WrappedModel Model


type alias Model =
    { entirePersons : Dict String EntirePerson
    , entireAddresses : Dict String EntireAddress
    , persons : Dict String Person
    , addresses : Dict String Address
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


(//) : Maybe a -> a -> a
(//) =
    flip Maybe.withDefault


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
    | ListenUnlisten ( Int, String, String )
    | ListenEvent ( Int, String, String )


type alias ConnectionInfo =
    { host : String
    , port' : Int
    , database : String
    , user : String
    , password : String
    }


connectionInfo : ConnectionInfo
connectionInfo =
    { host = "postgresDBServer"
    , port' = 5432
    , database = "test"
    , user = "charles"
    , password = "testpassword"
    }


initModel : Model
initModel =
    { entirePersons = Dict.empty
    , entireAddresses = Dict.empty
    , persons = Dict.empty
    , addresses = Dict.empty
    , engineModel = Engine.initModel connectionInfo.host connectionInfo.port' connectionInfo.database connectionInfo.user connectionInfo.password
    , queries = Dict.empty
    , didRefresh = False
    , listenConnectionId = Nothing
    }


executeQuery : Maybe String -> Query Msg -> List String -> Result (List String) ( Engine.Model Msg, Cmd Msg, Int )
executeQuery =
    Engine.executeQuery EngineError EventProcessingError EventProcessingComplete SlateEngine initModel.engineModel


refreshQuery : Engine.Model Msg -> Int -> ( Engine.Model Msg, Cmd Msg )
refreshQuery =
    Engine.refreshQuery SlateEngine


init : ( Model, Cmd Msg )
init =
    let
        result =
            executeQuery Nothing personQuery [ "123", "456" ]

        -- executeQuery (Just "id NOT IN (3, 7)") personQuery [ "123", "456" ]
    in
        case result of
            Ok ( engineModel, cmd, queryId ) ->
                { initModel | engineModel = engineModel, queries = Dict.insert queryId projectPersonQuery initModel.queries }
                    ! [ cmd
                      , Postgres.connect connectionInfo.host connectionInfo.port' connectionInfo.database connectionInfo.user connectionInfo.password ConnectError Connect ConnectionLost
                      ]

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


mutationError : String -> Model -> (String -> ( Model, Cmd Msg ))
mutationError type' model =
    (\err -> update (MutationError type' err) model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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

                ( ( engineModel, engineCmd ), appMsgs ) =
                    Engine.update engineMsg model.engineModel

                doUpdate msg ( model, cmds ) =
                    let
                        ( newModel, newCmd ) =
                            update msg model
                    in
                        ( newModel, newCmd :: cmds )

                ( newModel, myCmds ) =
                    List.foldl doUpdate ( { model | engineModel = engineModel }, [] ) appMsgs

                myCmd =
                    myCmds
                        |> List.filter ((/=) Cmd.none)
                        |> Cmd.batch
            in
                newModel ! [ myCmd, Cmd.map SlateEngine engineCmd ]

        MutatePerson eventRecord ->
            Result.map (\newDict -> { model | entirePersons = newDict } ! [])
                (PersonEntity.handleMutation model.entirePersons model.entireAddresses eventRecord.event)
                /// mutationError "Person" model

        MutateAddress eventRecord ->
            Result.map (\newDict -> { model | entireAddresses = newDict } ! [])
                (AddressEntity.handleMutation model.entireAddresses eventRecord.event)
                /// mutationError "Address" model

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

                unknownQueryId =
                    "Unknown query id: " ++ (toString queryId)

                projectionResult : Result (List String) WrappedModel
                projectionResult =
                    (Maybe.map (\projection -> projection <| WrappedModel model) <| Dict.get queryId model.queries) // (Err [ unknownQueryId ])

                crash =
                    if isErr projectionResult then
                        getErr projectionResult [ unknownQueryId ]
                            |> String.join "\n"
                            |> Debug.crash
                    else
                        ""

                newModel =
                    ((Result.map (\wrappedModel -> unwrapModel wrappedModel) projectionResult) /// (\_ -> model))

                ( newEngineModel, cmd ) =
                    if newModel.didRefresh == False then
                        refreshQuery newModel.engineModel queryId
                    else
                        -- let
                        --     json =
                        --         Debug.log "json" <| Engine.exportQueryState newModel.engineModel queryId
                        --
                        --     import' =
                        --         Engine.importQueryState EngineError EventProcessingError EventProcessingComplete SlateEngine
                        --
                        --     result =
                        --         Debug.log "import" <| import' personQuery newModel.engineModel json
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

        ListenUnlisten ( connectionId, channel, type' ) ->
            let
                l =
                    Debug.log "ListenUnlisten" ( connectionId, channel, type' )
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
            projectMap (toPerson <| okAddressesOnly addresses) model.entirePersons

        newModel =
            { model | persons = okPersonsOnly persons, addresses = okAddressesOnly addresses }

        allErrors =
            allProjectionErrors [ projectionErrors addresses, projectionErrors persons ]
    in
        if allErrors == [] then
            Ok <| WrappedModel newModel
        else
            Err allErrors


okAddressesOnly : Dict comparable (Result x Address) -> Dict comparable Address
okAddressesOnly =
    okOnly defaultAddress


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
        { street = entireAddress.street // defaultAddress.street
        }


okPersonsOnly : Dict comparable (Result x Person) -> Dict comparable Person
okPersonsOnly =
    okOnly defaultPerson


defaultPerson : Person
defaultPerson =
    { name = defaultEntirePerson.name
    , address = defaultAddress
    }


{-| convert entire person to person
-}
toPerson : Dict String Address -> EntirePerson -> Result (List String) Person
toPerson addresses entirePerson =
    let
        addressRef =
            entirePerson.address

        address =
            MaybeE.join <| Maybe.map (\ref -> Dict.get ref addresses) addressRef

        getPerson : Address -> Result (List String) Person
        getPerson address =
            getValidEntity
                [ ( isNothing entirePerson.name, "name is missing" )
                ]
                { name = entirePerson.name // defaultPerson.name
                , address = address
                }
    in
        if isNothing address && (not <| isNothing addressRef) then
            Err [ "Cannot find address id: " ++ (addressRef // "BUG") ]
        else
            getPerson <| address // defaultAddress


query : NodeQuery Msg
query =
    Slate.Query.query MissingMutationMsg


personQuery : Query Msg
personQuery =
    Node { query | schema = personSchema, properties = Just [ "name" ], msg = MutatePerson }
        [ Leaf { query | schema = addressSchema, properties = Just [ "street" ], msg = MutateAddress } ]


emptyEventData : EventData
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
    let
        channel =
            "eventsinsert"
    in
        Maybe.map (\connectionId -> Postgres.listen (model.listenConnectionId // 1) channel (ListenUnlistenError channel) ListenUnlisten ListenEvent) model.listenConnectionId
            // Sub.none



-- {-| subscribe to input from JS and the clock ticks every second
-- -}
-- subscriptions : Model -> Sub Msg
-- subscriptions _ =
--     Sub.batch
--         ([ Time.every 10000 Tick
--          ]
--         )
