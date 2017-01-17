module Slate.Engine
    exposing
        ( Config
        , Model
        , Msg
        , update
        , initModel
        , executeQuery
        , refreshQuery
        , importQueryState
        , exportQueryState
        )

import String exposing (..)
import Dict exposing (..)
import Set exposing (..)
import Json.Decode as JD exposing (..)
import Json.Encode as JE exposing (..)
import Utils.Json as JsonU exposing ((///), (<||))
import List.Extra as ListE exposing (..)
import Regex exposing (HowMany(All, AtMost))
import Utils.Regex as RegexU
import DebugF exposing (..)
import Slate.Query exposing (Query(..), MessageDict, MessageDictEntry, AppEventMsg, buildQueryTemplate, buildMessageDict, parametricReplace)
import Slate.Event exposing (EventRecord, Event, eventRecordDecoder)
import Utils.Ops exposing (..)
import Utils.Tuple exposing (..)
import Postgres exposing (..)


{-| - TODO change this !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-}
queryBatchSize : Int
queryBatchSize =
    2


type alias ErrorMsg msg =
    ( Int, String ) -> msg


type alias EventProcessingErrorMsg msg =
    ( String, String ) -> msg


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
    , messageDict : MessageDict msg
    }


type alias Config msg =
    { host : String
    , port_ : Int
    , database : String
    , user : String
    , password : String
    , errorMsg : ErrorMsg msg
    , eventProcessingErrorMsg : EventProcessingErrorMsg msg
    , completionMsg : Int -> msg
    , tagger : Msg -> msg
    }


type alias Model msg =
    { nextId : Int
    , queryStates : Dict Int (QueryState msg)
    }


initModel : Model msg
initModel =
    { nextId = 0
    , queryStates = Dict.empty
    }


type Msg
    = Nop
    | ConnectError Int ( Int, String )
    | Connect Int Int
    | ConnectionLost Int ( Int, String )
    | DisconnectError Int ( Int, String )
    | Disconnect Int Int
    | Events Int ( Int, List String )
    | QueryError Int ( Int, String )


getQueryState : Int -> Model msg -> QueryState msg
getQueryState queryStateId model =
    case Dict.get queryStateId model.queryStates of
        Just queryState ->
            queryState

        Nothing ->
            Debug.crash <| "Query Id: " ++ (toString queryStateId) ++ " is not in model: " ++ (toString model)


connectionFailure : Config msg -> Model msg -> Int -> String -> ( ( Model msg, Cmd Msg ), List msg )
connectionFailure config model queryStateId error =
    let
        queryState =
            getQueryState queryStateId model
    in
        ( model ! [], [ config.errorMsg <| ( queryStateId, error ) ] )


update : Config msg -> Msg -> Model msg -> ( ( Model msg, Cmd Msg ), List msg )
update config msg model =
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
                    startQuery model queryStateId connectionId
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
                                    startQuery updatedModel queryStateId connectionId

                                False ->
                                    ( updatedModel, nextQuery queryStateId connectionId )

                        False ->
                            model ! []

                {- handle end of query -}
                endOfQuery =
                    cmd == Cmd.none

                ( finalMsgs, finalCmd ) =
                    endOfQuery ? ( ( List.append msgs [ config.completionMsg queryStateId ], Postgres.disconnect (DisconnectError queryStateId) (Disconnect queryStateId) connectionId False ), ( msgs, cmd ) )
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


templateReplace : List ( String, String ) -> String -> String
templateReplace =
    parametricReplace "{{" "}}"


quoteList : List String -> List String
quoteList =
    List.map (\s -> "'" ++ s ++ "'")


startQuery : Model msg -> Int -> Int -> ( Model msg, Cmd Msg )
startQuery model queryStateId connectionId =
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
                                noIds =
                                    ids == []

                                entityIdClause =
                                    noIds ? ( "1=1", "event #>> '{data, entityId}' IN (" ++ (String.join ", " <| quoteList ids) ++ ")" )
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
                        , Postgres.query (QueryError queryStateId) (Events queryStateId) connectionId sql queryBatchSize
                        )
                )
            ?= ( updateQueryState model { queryState | first = False }, Cmd.none )


processEvents : Config msg -> Model msg -> Int -> List String -> ( Model msg, List msg )
processEvents config model queryStateId eventStrs =
    let
        queryState =
            getQueryState queryStateId model

        eventNotInDict =
            "Event not in message dictionary: " ++ (toString queryState.messageDict)

        missingReferenceValue event =
            "Event referenceId is missing: " ++ (toString event)

        eventError eventStr msgs error =
            ( { queryState | badQueryState = True }, config.eventProcessingErrorMsg ( eventStr, error ) :: msgs )

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
                                                    Dict.get event.name queryState.messageDict
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
                                                                                    event.data.referenceId
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


nextQuery : Int -> Int -> Cmd Msg
nextQuery queryStateId connectionId =
    Postgres.moreQueryResults (QueryError queryStateId) (Events queryStateId) connectionId


connectToDb : Config msg -> Int -> Cmd msg
connectToDb config queryStateId =
    Postgres.connect (config.tagger << (ConnectError queryStateId)) (config.tagger << (Connect queryStateId)) (config.tagger << (ConnectionLost queryStateId)) 15000 config.host config.port_ config.database config.user config.password



-- Public API


executeQuery : Config msg -> Model msg -> Maybe String -> Query msg -> List String -> Result (List String) ( Model msg, Cmd msg, Int )
executeQuery config model additionalCriteria query rootIds =
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
                            , messageDict = buildMessageDict query
                            , first = True
                            }

                        cmd =
                            connectToDb config queryStateId
                    in
                        ( { model | nextId = model.nextId + 1, queryStates = Dict.insert queryStateId queryState model.queryStates }, cmd, queryStateId )
                )


refreshQuery : Config msg -> Model msg -> Int -> ( Model msg, Cmd msg )
refreshQuery config model queryStateId =
    let
        queryState =
            getQueryState queryStateId model

        newQueryState =
            { queryState | currentTemplate = 0, firstTemplateWithDataMaxId = -1 }

        newModel =
            { model | queryStates = Dict.insert queryStateId newQueryState model.queryStates }
    in
        ( newModel, connectToDb config queryStateId )


closeQuery : Model msg -> Int -> Model msg
closeQuery model queryStateId =
    { model | queryStates = Dict.remove queryStateId model.queryStates }


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


exportQueryState : Model msg -> Int -> String
exportQueryState model queryStateId =
    let
        maybeQueryState =
            Dict.get queryStateId model.queryStates
    in
        maybeQueryState
            |?> (\queryState -> queryStateEncode queryState)
            ?= ""


queryStateDecode : MessageDict msg -> String -> Result String (QueryState msg)
queryStateDecode messageDict json =
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
            <|| JD.succeed messageDict
        )
        json


importQueryState : Query msg -> Model msg -> String -> Result String (Model msg)
importQueryState query model json =
    let
        templateResult =
            buildQueryTemplate query
    in
        (queryStateDecode (buildMessageDict query) json)
            |??>
                (\queryState ->
                    let
                        queryStateId =
                            model.nextId
                    in
                        { model | nextId = model.nextId + 1, queryStates = Dict.insert queryStateId queryState model.queryStates }
                )