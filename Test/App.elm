port module Test.App exposing (..)

import String exposing (..)
import StringUtils exposing (..)
import Dict exposing (Dict)
import Maybe.Extra as MaybeE exposing (isNothing)
import Slate.TestEntities.PersonEntity as PersonEntity exposing (EntirePerson, EntirePersonDict, defaultEntirePerson)
import Slate.TestEntities.AddressEntity as AddressEntity exposing (EntireAddress, EntireAddressDict, defaultEntireAddress)
import Slate.TestEntities.PersonSchema as PersonSchema exposing (..)
import Slate.TestEntities.AddressSchema as AddressSchema exposing (..)
import Slate.Engine.Query exposing (..)
import Slate.Engine.Engine as Engine exposing (..)
import Slate.Common.Event exposing (..)
import Slate.Common.Projection exposing (..)
import Slate.Common.Entity exposing (..)
import Slate.Common.Mutation as Mutation exposing (CascadingDeletionTaggers)
import Slate.Common.Db exposing (..)
import Utils.Ops exposing (..)
import Utils.Error exposing (..)
import Utils.Log exposing (..)


-- import Postgres exposing (..)

import ParentChildUpdate exposing (..)


port node : Float -> Cmd msg


{-|
    Avoid infinitely recursive definition in Model.
-}
type WrappedModel
    = WrappedModel Model


type alias PersonDict =
    EntityDict Person


type alias AddressDict =
    EntityDict Address


type alias Model =
    { entirePersons : EntirePersonDict
    , entireAddresses : EntireAddressDict
    , persons : PersonDict
    , addresses : AddressDict
    , engineModel : Engine.Model Msg
    , queries : Dict Int (WrappedModel -> Result (ProjectionErrors) WrappedModel)
    , didRefresh :
        Bool
        -- , listenConnectionId : Maybe Int
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
    | EngineModule Engine.Msg
    | EventError EventRecord ( Int, String )
    | EngineLog ( LogLevel, ( Int, String ) )
    | EngineError ( ErrorType, ( Int, String ) )
    | EventProcessingComplete Int
    | MutationError String String
    | UnspecifiedMutationInQuery EventRecord
    | MutatePerson EventRecord
    | MutateAddress EventRecord
    | EventProcessingError ( String, String )


engineDBInfo : DbConnectionInfo
engineDBInfo =
    { host = "postgresDBServer"
    , port_ = 5432
    , database = "test_entities"
    , user = "charles"
    , password = "testpassword"
    , timeout = 15000
    }


engineConfig : Engine.Config Msg
engineConfig =
    { logTagger = EngineLog
    , errorTagger = EngineError
    , eventProcessingErrorTagger = EventProcessingError
    , completionTagger = EventProcessingComplete
    , routeToMeTagger = EngineModule
    , queryBatchSize =
        2
        -- TODO CHANGE THIS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
    }


executeQuery : Engine.Model Msg -> Maybe String -> Query Msg -> List String -> Result (List String) ( Engine.Model Msg, Cmd Msg, Int )
executeQuery =
    Engine.executeQuery engineConfig engineDBInfo


refreshQuery : Engine.Model Msg -> Int -> Result String ( Engine.Model Msg, Cmd Msg )
refreshQuery =
    Engine.refreshQuery engineConfig engineDBInfo


init : ( Model, Cmd Msg )
init =
    let
        result =
            executeQuery initModel.engineModel Nothing personQuery [ "123", "456" ]

        -- executeQuery initModel.engineModel (Just "id NOT IN (3, 7)") personQuery [ "123", "456" ]
    in
        result
            |??>
                (\( engineModel, cmd, queryId ) ->
                    { initModel | engineModel = engineModel, queries = Dict.insert queryId projectPerson initModel.queries } ! [ cmd ]
                )
            ??= (\errs -> Debug.crash <| "Init error: " ++ (String.join "\n" errs))


main : Program Never Model Msg
main =
    Platform.program
        { init = init
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
            ParentChildUpdate.updateChildApp (Engine.update engineConfig) update .engineModel engineConfig.routeToMeTagger (\model engineModel -> { model | engineModel = engineModel })

        processCascadingMutationResult =
            Mutation.processCascadingMutationResult model
                deleteTaggers
                MutationError
                update

        processMutationResult =
            Mutation.processMutationResult model
    in
        case msg of
            Nop ->
                model ! []

            MutatePerson eventRecord ->
                PersonEntity.handleMutation model.entirePersons model.entireAddresses eventRecord.event
                    |> processCascadingMutationResult
                        eventRecord
                        (\model newDict -> { model | entirePersons = Debug.log "New Person Dictionary" newDict })
                        (mutationError "Person")

            MutateAddress eventRecord ->
                let
                    l =
                        Debug.log "MutateAddress" eventRecord
                in
                    AddressEntity.handleMutation model.entireAddresses eventRecord.event
                        |> processMutationResult
                            (\model newDict -> { model | entireAddresses = Debug.log "New Address Dictionary" newDict })
                            (mutationError "Address")

            EngineLog ( level, ( queryId, err ) ) ->
                let
                    l =
                        Debug.log "EngineLog" ( level, ( queryId, err ) )
                in
                    model ! []

            EngineError ( type_, ( queryId, err ) ) ->
                let
                    l =
                        Debug.log "EngineError" ( type_, ( queryId, err ) )
                in
                    model ! []

            EngineModule engineMsg ->
                updateEngine engineMsg model

            EventProcessingComplete queryId ->
                let
                    l =
                        Debug.log "EventProcessingComplete" ""

                    projectionResult =
                        Dict.get queryId model.queries
                            |?> (\projection -> projection <| WrappedModel model)
                            ?= (Err [ "Unknown query id: " ++ (toString queryId) ])

                    crash =
                        projectionResult |??> identity ??= (Debug.crash << String.join "\n")

                    newModel =
                        projectionResult |??> (\wrappedModel -> unwrapModel wrappedModel) ??= (\_ -> model)

                    ( newEngineModel, cmd ) =
                        newModel.didRefresh
                            ? ( refreshQuery newModel.engineModel queryId
                                    |??> identity
                                    ??= (\error -> Debug.crash ("refreshQuery:" +-+ error))
                              , -- let
                                --         json =
                                --             Debug.log "json" <| Engine.exportQueryState newModel.engineModel queryId |??> identity ??= (\error -> Debug.crash "exportQueryState:" +-+ error)
                                --
                                --         result =
                                --             Debug.log "import" <| Engine.importQueryState personQuery newModel.engineModel json
                                --     in
                                ( newModel.engineModel, Cmd.none )
                              )
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

            UnspecifiedMutationInQuery eventRecord ->
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


unwrapModel : WrappedModel -> Model
unwrapModel wrappedModel =
    case wrappedModel of
        WrappedModel model ->
            model


projectPerson : WrappedModel -> Result (ProjectionErrors) WrappedModel
projectPerson wrappedModel =
    let
        model =
            unwrapModel wrappedModel

        addressProjections =
            projectMap toAddress model.entireAddresses

        personProjections =
            projectMap (toPerson <| successfulProjections addressProjections) model.entirePersons

        newModel =
            { model | persons = successfulProjections personProjections, addresses = successfulProjections addressProjections }

        allErrors =
            allFailedProjections [ failedProjections addressProjections, failedProjections personProjections ]
    in
        (allErrors == []) ? ( Ok <| WrappedModel newModel, Err allErrors )


defaultAddress : Address
defaultAddress =
    { street = defaultEntireAddress.street
    }


{-|
    Convert entire address to address
-}
toAddress : EntireAddress -> Result (ProjectionErrors) Address
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
toPerson : AddressDict -> EntirePerson -> Result (ProjectionErrors) Person
toPerson addresses entirePerson =
    let
        maybeAddressRef =
            entirePerson.address

        maybeAddress =
            MaybeE.join <| maybeAddressRef |?> (\ref -> Dict.get ref addresses)

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
    Slate.Engine.Query.mtQuery UnspecifiedMutationInQuery


personQuery : Query Msg
personQuery =
    Node { query | schema = personSchema, properties = Just [ "name" ], tagger = MutatePerson }
        [ Leaf { query | schema = addressSchema, properties = Just [ "street" ], tagger = MutateAddress } ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



{-
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
-}
