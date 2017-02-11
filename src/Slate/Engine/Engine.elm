module Slate.Engine.Engine
    exposing
        ( QueryStateId
        , Config
        , Model
        , Msg
        , initModel
        , update
        , executeQuery
        , refreshQuery
        , disposeQuery
        , importQueryState
        , exportQueryState
        )

{-|
    Slate Query Engine.

@docs QueryStateId, Config , Model , Msg , initModel , update , executeQuery , refreshQuery , disposeQuery , importQueryState , exportQueryState
-}

import String exposing (..)
import StringUtils exposing (..)
import Dict exposing (..)
import Set exposing (..)
import Json.Decode as JD exposing (..)
import Json.Encode as JE exposing (..)
import Utils.Json as JsonU exposing ((///), (<||))
import List.Extra as ListE exposing (..)
import Regex exposing (HowMany(All, AtMost))
import Utils.Regex as RegexU
import DebugF exposing (..)
import Slate.Engine.Query exposing (Query(..), MsgDict, MsgDictEntry, buildQueryTemplate, buildMsgDict, parametricReplace)
import Slate.Common.Event exposing (EventRecord, Event, EventData(..), eventRecordDecoder)
import Slate.Common.Db exposing (..)
import Utils.Ops exposing (..)
import Utils.Tuple exposing (..)
import Utils.Error exposing (..)
import Utils.Log exposing (..)
import Postgres exposing (..)
import ParentChildUpdate
import Retry exposing (FailureTagger)


---------------------------------------------------------------------------------------------------------
-- PUBLIC API
---------------------------------------------------------------------------------------------------------


{-|
    QueryState id.
-}
type alias QueryStateId =
    Int


{-|
    Slate Engine configuration.
-}
type alias Config msg =
    { logTagger : ( LogLevel, ( QueryStateId, String ) ) -> msg
    , errorTagger : ( ErrorType, ( QueryStateId, String ) ) -> msg
    , eventProcessingErrorTagger : ( String, String ) -> msg
    , completionTagger : QueryStateId -> msg
    , routeToMeTagger : Msg -> msg
    , queryBatchSize : Int
    }


{-|
    Engine Model.
-}
type alias Model msg =
    { nextId : QueryStateId
    , queryStates : Dict QueryStateId (QueryState msg)
    , retryModel : Retry.Model Msg
    }


{-|
    Engine Msg.
-}
type Msg
    = Nop
    | ConnectError QueryStateId ( ConnectionId, String )
    | Connect QueryStateId ConnectionId
    | ConnectionLost QueryStateId ( ConnectionId, String )
    | DisconnectError QueryStateId ( ConnectionId, String )
    | Disconnect QueryStateId ConnectionId
    | Events QueryStateId ( ConnectionId, List String )
    | QueryError QueryStateId ( ConnectionId, String )
    | RetryConnectCmd Int Msg (Cmd Msg)
    | RetryModule (Retry.Msg Msg)


{-|
    Initial model for the Engine.
-}
initModel : Model msg
initModel =
    { nextId = 0
    , queryStates = Dict.empty
    , retryModel = Retry.initModel
    }


{-|
    Engine's update function.
-}
update : Config msg -> Msg -> Model msg -> ( ( Model msg, Cmd Msg ), List msg )
update config msg model =
    let
        logMsg queryStateId message =
            config.logTagger ( LogLevelInfo, ( queryStateId, message ) )

        nonFatal queryStateId error =
            config.errorTagger ( NonFatalError, ( queryStateId, error ) )

        fatal queryStateId error =
            config.errorTagger ( FatalError, ( queryStateId, error ) )

        updateRetry =
            ParentChildUpdate.updateChildParent (Retry.update retryConfig) (update config) .retryModel RetryModule (\model retryModel -> { model | retryModel = retryModel })
    in
        case msg of
            Nop ->
                ( model ! [], [] )

            ConnectError queryStateId ( connectionId, error ) ->
                let
                    l =
                        Debug.log "ConnectError" ( queryStateId, connectionId, error )
                in
                    connectionFailure config model queryStateId error

            ConnectionLost queryStateId ( connectionId, error ) ->
                let
                    l =
                        Debug.log "ConnectLost" ( queryStateId, connectionId, error )
                in
                    connectionFailure config model queryStateId error

            Connect queryStateId connectionId ->
                let
                    l =
                        Debug.log "Connect" ( queryStateId, connectionId )

                    queryState =
                        getQueryState queryStateId model

                    ( newModel, cmd ) =
                        startQuery config model queryStateId connectionId
                in
                    ( newModel ! [ cmd ], [] )

            DisconnectError queryStateId ( connectionId, error ) ->
                let
                    l =
                        Debug.log "DisconnectError" ( queryStateId, connectionId, error )

                    queryState =
                        getQueryState queryStateId model
                in
                    connectionFailure config model queryStateId error

            Disconnect queryStateId connectionId ->
                let
                    l =
                        Debug.log "Disconnect" ( queryStateId, connectionId )
                in
                    ( model ! [], [] )

            Events queryStateId ( connectionId, eventStrs ) ->
                let
                    l =
                        DebugF.log "Event" eventStrs

                    ll =
                        DebugF.log "Model" model

                    ( updatedModel, msgs ) =
                        processEvents config model queryStateId eventStrs

                    queryState =
                        getQueryState queryStateId model

                    goodQueryState =
                        queryState.badQueryState == False

                    startOfQuery =
                        eventStrs == []

                    ( finalModel, cmd ) =
                        case goodQueryState of
                            True ->
                                case startOfQuery of
                                    True ->
                                        startQuery config updatedModel queryStateId connectionId

                                    False ->
                                        ( updatedModel, nextQuery queryStateId connectionId )

                            False ->
                                model ! []

                    {- handle end of query -}
                    endOfQuery =
                        cmd == Cmd.none

                    ( finalMsgs, finalCmd ) =
                        endOfQuery ? ( ( List.append msgs [ config.completionTagger queryStateId ], Postgres.disconnect (DisconnectError queryStateId) (Disconnect queryStateId) connectionId False ), ( msgs, cmd ) )
                in
                    ( finalModel ! [ finalCmd ], finalMsgs )

            QueryError queryStateId ( connectionId, error ) ->
                let
                    l =
                        Debug.log "QueryError" ( queryStateId, connectionId, error )

                    queryState =
                        getQueryState queryStateId model
                in
                    connectionFailure config model queryStateId error

            RetryConnectCmd retryCount failureMsg cmd ->
                let
                    parentMsg =
                        case failureMsg of
                            ConnectError queryStateId ( connectionId, error ) ->
                                nonFatal queryStateId ("Connection Error:" +-+ error +-+ "Connection Retry:" +-+ retryCount)

                            _ ->
                                Debug.crash "BUG -- Should never get here"
                in
                    ( model ! [ cmd ], [ parentMsg ] )

            RetryModule msg ->
                updateRetry msg model


{-|
   Execute Slate Query.
-}
executeQuery : Config msg -> DbConnectionInfo -> Model msg -> Maybe String -> Query msg -> List String -> Result (List String) ( Model msg, Cmd msg, Int )
executeQuery config dbInfo model additionalCriteria query rootIds =
    let
        templateResult =
            buildQueryTemplate query
    in
        templateResult
            |??>
                (\templates ->
                    let
                        queryStateId =
                            model.nextId

                        rootEntity =
                            case query of
                                Node nodeQuery _ ->
                                    nodeQuery.schema.type_

                                Leaf nodeQuery ->
                                    nodeQuery.schema.type_

                        queryState =
                            { rootEntity = rootEntity
                            , badQueryState = False
                            , currentTemplate = 0
                            , templates = templates
                            , rootIds = rootIds
                            , ids = Dict.empty
                            , additionalCriteria = additionalCriteria
                            , maxIds = Dict.empty
                            , firstTemplateWithDataMaxId = -1
                            , msgDict = buildMsgDict query
                            , first = True
                            }

                        ( newModel, cmd ) =
                            connectToDb config dbInfo { model | nextId = model.nextId + 1, queryStates = Dict.insert queryStateId queryState model.queryStates } queryStateId
                    in
                        ( newModel, cmd, queryStateId )
                )


{-|
    Refresh an existing Slate Query, i.e. process events since the last `executeQuery` or `refreshQuery`.
-}
refreshQuery : Config msg -> DbConnectionInfo -> Model msg -> QueryStateId -> ( Model msg, Cmd msg )
refreshQuery config dbInfo model queryStateId =
    let
        queryState =
            getQueryState queryStateId model

        newQueryState =
            { queryState | currentTemplate = 0, firstTemplateWithDataMaxId = -1 }

        ( newModel, cmd ) =
            connectToDb config dbInfo { model | queryStates = Dict.insert queryStateId newQueryState model.queryStates } queryStateId
    in
        ( newModel, cmd )


{-|
    Stop managing specified `Query`. Afterwards, the `queryStateId` will no longer be valid.
-}
disposeQuery : Model msg -> QueryStateId -> Model msg
disposeQuery model queryStateId =
    { model | queryStates = Dict.remove queryStateId model.queryStates }


{-|
    Create a JSON String for saving the specified `QueryState`.

    This is useful for caching a query or saving for a subsequent execution of your App.
-}
exportQueryState : Model msg -> QueryStateId -> String
exportQueryState model queryStateId =
    let
        maybeQueryState =
            Dict.get queryStateId model.queryStates
    in
        maybeQueryState
            |?> (\queryState -> queryStateEncode queryState)
            ?= ""


{-|
    Recreate a previously saved `QueryState` from the specified JSON String.
-}
importQueryState : Query msg -> Model msg -> String -> Result String (Model msg)
importQueryState query model json =
    let
        templateResult =
            buildQueryTemplate query
    in
        (queryStateDecode (buildMsgDict query) json)
            |??>
                (\queryState ->
                    let
                        queryStateId =
                            model.nextId
                    in
                        { model | nextId = model.nextId + 1, queryStates = Dict.insert queryStateId queryState model.queryStates }
                )



---------------------------------------------------------------------------------------------------------
-- PRIVATE
---------------------------------------------------------------------------------------------------------


retryConfig : Retry.Config Msg
retryConfig =
    { retryMax = 3
    , delayNext = Retry.constantDelay 5000
    , routeToMeTagger = RetryModule
    }


type alias QueryState msg =
    { first : Bool
    , rootEntity : String
    , badQueryState : Bool
    , currentTemplate : Int
    , templates : List String
    , rootIds : List String
    , ids : Dict String (Set String)
    , additionalCriteria : Maybe String
    , maxIds : Dict String Int
    , firstTemplateWithDataMaxId : Int
    , msgDict : MsgDict msg
    }


queryStateEncode : QueryState msg -> String
queryStateEncode queryState =
    JE.encode 0 <|
        JE.object
            [ ( "first", JE.bool queryState.first )
            , ( "rootEntity", JE.string queryState.rootEntity )
            , ( "badQueryState", JE.bool queryState.badQueryState )
            , ( "currentTemplate", JE.int queryState.currentTemplate )
            , ( "templates", JE.list <| List.map JE.string queryState.templates )
            , ( "rootIds", JE.list <| List.map JE.string queryState.rootIds )
            , ( "ids", JsonU.encDict JE.string (JE.list << List.map JE.string << Set.toList) queryState.ids )
            , ( "additionalCriteria", JsonU.encMaybe JE.string queryState.additionalCriteria )
            , ( "maxIds", JsonU.encDict JE.string JE.int queryState.maxIds )
            , ( "firstTemplateWithDataMaxId", JE.int queryState.firstTemplateWithDataMaxId )
            ]


queryStateDecode : MsgDict msg -> String -> Result String (QueryState msg)
queryStateDecode msgDict json =
    JD.decodeString
        ((JD.succeed QueryState)
            <|| ("first" := JD.bool)
            <|| ("rootEntity" := JD.string)
            <|| ("badQueryState" := JD.bool)
            <|| ("currentTemplate" := JD.int)
            <|| ("templates" := JD.list JD.string)
            <|| ("rootIds" := JD.list JD.string)
            <|| ("ids" := JsonU.decConvertDict Set.fromList JD.string (JD.list JD.string))
            <|| ("additionalCriteria" := JD.maybe JD.string)
            <|| ("maxIds" := JsonU.decDict JD.string JD.int)
            <|| ("firstTemplateWithDataMaxId" := JD.int)
            <|| JD.succeed msgDict
        )
        json


getQueryState : QueryStateId -> Model msg -> QueryState msg
getQueryState queryStateId model =
    case Dict.get queryStateId model.queryStates of
        Just queryState ->
            queryState

        Nothing ->
            Debug.crash <| "Query Id: " ++ (toString queryStateId) ++ " is not in model: " ++ (toString model)


connectionFailure : Config msg -> Model msg -> QueryStateId -> String -> ( ( Model msg, Cmd Msg ), List msg )
connectionFailure config model queryStateId error =
    let
        queryState =
            getQueryState queryStateId model
    in
        ( model ! [], [ config.errorTagger ( NonFatalError, ( queryStateId, error ) ) ] )


templateReplace : List ( String, String ) -> String -> String
templateReplace =
    parametricReplace "{{" "}}"


quoteList : List String -> List String
quoteList =
    List.map (\s -> "'" ++ s ++ "'")


startQuery : Config msg -> Model msg -> QueryStateId -> ConnectionId -> ( Model msg, Cmd Msg )
startQuery config model queryStateId connectionId =
    let
        queryState =
            getQueryState queryStateId model

        maybeTemplate =
            List.head <| (List.drop queryState.currentTemplate queryState.templates)

        firstTemplate =
            queryState.currentTemplate == 0

        haveFirstTemplateMaxId =
            queryState.firstTemplateWithDataMaxId /= -1

        maxIdColumn =
            haveFirstTemplateMaxId ? ( "", ", q.max" )

        maxIdSQLClause =
            haveFirstTemplateMaxId ? ( "", "CROSS JOIN (SELECT MAX(id) FROM events) AS q\n" )

        firstTemplateWithDataMaxCriteria =
            haveFirstTemplateMaxId ? ( "AND id < " ++ (toString queryState.firstTemplateWithDataMaxId), "" )

        entityIds : Dict String (Set String)
        entityIds =
            firstTemplate ? ( Dict.insert queryState.rootEntity (Set.fromList queryState.rootIds) Dict.empty, queryState.ids )

        lastMaxId =
            toString (queryState.first ? ( -1, (ListE.foldl1 max <| Dict.values queryState.maxIds) ?= -1 ))

        updateQueryState model queryState =
            { model | queryStates = Dict.insert queryStateId queryState model.queryStates }
    in
        maybeTemplate
            |?> (\template ->
                    let
                        sqlTemplate =
                            templateReplace
                                [ ( "additionalCriteria", queryState.additionalCriteria ?= "1=1" )
                                , ( "firstTemplateWithDataMaxCriteria", firstTemplateWithDataMaxCriteria )
                                , ( "maxIdColumn", maxIdColumn )
                                , ( "maxIdSQLClause", maxIdSQLClause )
                                , ( "lastMaxId", lastMaxId )
                                ]
                                template

                        replace entityType ids =
                            let
                                entityIdClause =
                                    (ids == []) ? ( "1=1", "event #>> '{data, entityId}' IN (" ++ (String.join ", " <| quoteList ids) ++ ")" )
                            in
                                templateReplace [ ( entityType ++ "-entityIds", entityIdClause ) ]

                        sqlWithEntityIds =
                            List.foldl (\( type_, ids ) template -> replace type_ ids template) sqlTemplate (secondMap Set.toList <| Dict.toList entityIds)

                        sql =
                            RegexU.replace All "\\{\\{.+?\\-entityIds\\}\\}" (RegexU.simpleReplacer "1??=1") sqlWithEntityIds

                        ll =
                            DebugF.log "sql" sql
                    in
                        ( updateQueryState model { queryState | currentTemplate = queryState.currentTemplate + 1 }
                        , Postgres.query (QueryError queryStateId) (Events queryStateId) connectionId sql config.queryBatchSize
                        )
                )
            ?= ( updateQueryState model { queryState | first = False }, Cmd.none )


processEvents : Config msg -> Model msg -> QueryStateId -> List String -> ( Model msg, List msg )
processEvents config model queryStateId eventStrs =
    let
        queryState =
            getQueryState queryStateId model

        eventNotInDict =
            "Event not in message dictionary: " ++ (toString queryState.msgDict)

        missingReferenceValue event =
            "Event referenceId is missing: " ++ (toString event)

        eventError eventStr msgs error =
            ( { queryState | badQueryState = True }, config.eventProcessingErrorTagger ( eventStr, error ) :: msgs )

        ( newQueryState, msgs ) =
            List.foldl
                (\eventStr ( queryState, msgs ) ->
                    let
                        eventRecordDecoded =
                            decodeString eventRecordDecoder eventStr
                    in
                        eventRecordDecoded
                            |??>
                                (\eventRecord ->
                                    let
                                        event =
                                            eventRecord.event

                                        resultRecordId =
                                            toInt eventRecord.id
                                    in
                                        resultRecordId
                                            |??>
                                                (\recordId ->
                                                    Dict.get event.name queryState.msgDict
                                                        |?> (\{ msg, maybeEntityType } ->
                                                                let
                                                                    firstTemplateWithDataMaxId =
                                                                        max queryState.firstTemplateWithDataMaxId ((Result.toMaybe <| toInt <| eventRecord.max ?= "-1") ?= -1)
                                                                in
                                                                    {- add referenced entities to ids dictionary for next SQL query -}
                                                                    maybeEntityType
                                                                        |?> (\entityType ->
                                                                                let
                                                                                    currentEntityMaxId =
                                                                                        (Dict.get entityType queryState.maxIds) ?= -1

                                                                                    entityMaxId =
                                                                                        max currentEntityMaxId recordId

                                                                                    ids =
                                                                                        Dict.get entityType queryState.ids ?= Set.empty
                                                                                in
                                                                                    case event.data of
                                                                                        Mutating eventData ->
                                                                                            eventData.referenceId
                                                                                                |?> (\referenceId ->
                                                                                                        ( { queryState
                                                                                                            | firstTemplateWithDataMaxId = firstTemplateWithDataMaxId
                                                                                                            , ids = Dict.insert entityType (Set.insert referenceId ids) queryState.ids
                                                                                                            , maxIds = Dict.insert entityType entityMaxId queryState.maxIds
                                                                                                          }
                                                                                                        , (msg eventRecord) :: msgs
                                                                                                        )
                                                                                                    )
                                                                                                ?= (eventError eventStr msgs <| missingReferenceValue eventRecord)

                                                                                        NonMutating _ ->
                                                                                            ( queryState, [] )
                                                                            )
                                                                        ?= ( { queryState | firstTemplateWithDataMaxId = firstTemplateWithDataMaxId }
                                                                           , (msg eventRecord) :: msgs
                                                                           )
                                                            )
                                                        ?= eventError eventStr msgs eventNotInDict
                                                )
                                            ??= (\_ -> eventError eventStr msgs "Corrupt Event Record -- Invalid Id")
                                )
                            ??= (\decodingErr -> eventError eventStr msgs decodingErr)
                )
                ( queryState, [] )
                eventStrs
    in
        ( { model | queryStates = Dict.insert queryStateId newQueryState model.queryStates }, List.reverse msgs )


nextQuery : QueryStateId -> ConnectionId -> Cmd Msg
nextQuery queryStateId connectionId =
    Postgres.moreQueryResults (QueryError queryStateId) (Events queryStateId) connectionId


connectToDbCmd : Config msg -> DbConnectionInfo -> Model msg -> QueryStateId -> FailureTagger ( ConnectionId, String ) Msg -> Cmd Msg
connectToDbCmd config dbInfo model queryStateId failureTagger =
    Postgres.connect
        failureTagger
        (Connect queryStateId)
        (ConnectionLost queryStateId)
        dbInfo.timeout
        dbInfo.host
        dbInfo.port_
        dbInfo.database
        dbInfo.user
        dbInfo.password


connectToDb : Config msg -> DbConnectionInfo -> Model msg -> QueryStateId -> ( Model msg, Cmd msg )
connectToDb config dbInfo model queryStateId =
    let
        ( retryModel, retryCmd ) =
            Retry.retry retryConfig model.retryModel (ConnectError queryStateId) RetryConnectCmd (connectToDbCmd config dbInfo model queryStateId)
    in
        { model | retryModel = retryModel } ! [ Cmd.map config.routeToMeTagger retryCmd ]
