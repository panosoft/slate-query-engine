module Slate.Engine exposing (Model, Msg, update, initModel, executeQuery, refreshQuery, importQueryState, exportQueryState)

import String exposing (..)
import Dict exposing (..)
import Set exposing (..)
import Json.Decode as JD exposing (..)
import Json.Encode as JE exposing (..)
import Json.Helper as Json exposing (..)
import List.Extra as ListE exposing (..)
import Maybe.Extra as MaybeE exposing (isNothing)
import Regex exposing (HowMany(All, AtMost))
import Regex.Extra as RegexE exposing (..)
import DebugF exposing (..)
import Slate.Query exposing (Query(..), MessageDict, MessageDictEntry, AppEventMsg, buildQueryTemplate, buildMessageDict, parametricReplace)
import Slate.Event exposing (EventRecord, Event, eventRecordDecoder)
import Utils.Utils exposing (..)
import Postgres exposing (..)


{-| - TODO change this !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-}
queryBatchSize : Int
queryBatchSize =
    10


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
    , errorMsg : ErrorMsg msg
    , eventProcessingErrorMsg : EventProcessingErrorMsg msg
    , completionMsg : Int -> msg
    , tagger : Msg -> msg
    , messageDict : MessageDict msg
    }


type alias Model msg =
    { nextId : Int
    , queryStates : Dict Int (QueryState msg)
    , host : String
    , port' : Int
    , database : String
    , user : String
    , password : String
    }


(//) : Maybe a -> a -> a
(//) =
    flip Maybe.withDefault


initModel : String -> Int -> String -> String -> String -> Model msg
initModel host port' database user password =
    { nextId = 0
    , queryStates = Dict.empty
    , host = host
    , port' = port'
    , database = database
    , user = user
    , password = password
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


connectionFailure : Int -> Model msg -> String -> ( ( Model msg, Cmd Msg ), List msg )
connectionFailure queryStateId model error =
    let
        queryState =
            getQueryState queryStateId model
    in
        ( model ! [], [ queryState.errorMsg <| ( queryStateId, error ) ] )


update : Msg -> Model msg -> ( ( Model msg, Cmd Msg ), List msg )
update msg model =
    case msg of
        Nop ->
            ( model ! [], [] )

        ConnectError queryStateId ( connectionId, error ) ->
            let
                l =
                    Debug.log "ConnectError" ( queryStateId, connectionId, error )
            in
                connectionFailure queryStateId model error

        ConnectionLost queryStateId ( connectionId, error ) ->
            let
                l =
                    Debug.log "ConnectLost" ( queryStateId, connectionId, error )
            in
                connectionFailure queryStateId model error

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
                connectionFailure queryStateId model error

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
                    processEvents model queryStateId eventStrs

                queryState =
                    getQueryState queryStateId model

                ( finalModel, cmd ) =
                    if queryState.badQueryState == False then
                        if eventStrs == [] then
                            startQuery updatedModel queryStateId connectionId
                        else
                            ( updatedModel, nextQuery queryStateId connectionId )
                    else
                        model ! []

                {- handle end of query -}
                endOfQuery =
                    cmd == Cmd.none

                ( finalMsgs, finalCmd ) =
                    if endOfQuery then
                        ( List.append msgs [ queryState.completionMsg queryStateId ], Postgres.disconnect connectionId False (DisconnectError queryStateId) (Disconnect queryStateId) )
                    else
                        ( msgs, cmd )
            in
                ( finalModel ! [ finalCmd ], finalMsgs )

        QueryError queryStateId ( connectionId, error ) ->
            let
                l =
                    Debug.log "QueryError" ( queryStateId, connectionId, error )

                queryState =
                    getQueryState queryStateId model
            in
                connectionFailure queryStateId model error


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
            if haveFirstTemplateMaxId then
                ""
            else
                ", q.max"

        maxIdSQLClause =
            if haveFirstTemplateMaxId then
                ""
            else
                "CROSS JOIN (SELECT MAX(id) FROM events) AS q\n"

        firstTemplateWithDataMaxCriteria =
            if haveFirstTemplateMaxId then
                "AND id < " ++ (toString queryState.firstTemplateWithDataMaxId)
            else
                ""

        entityIds : Dict String (Set String)
        entityIds =
            if firstTemplate then
                Dict.insert queryState.rootEntity (Set.fromList queryState.rootIds) Dict.empty
            else
                queryState.ids

        lastMaxId =
            toString
                (if queryState.first then
                    -1
                 else
                    (ListE.foldl1 max <| Dict.values queryState.maxIds) // -1
                )

        updateQueryState model queryState =
            { model | queryStates = Dict.insert queryStateId queryState model.queryStates }
    in
        (Maybe.map
            (\template ->
                let
                    sqlTemplate =
                        templateReplace
                            [ ( "additionalCriteria", queryState.additionalCriteria // "1=1" )
                            , ( "firstTemplateWithDataMaxCriteria", firstTemplateWithDataMaxCriteria )
                            , ( "maxIdColumn", maxIdColumn )
                            , ( "maxIdSQLClause", maxIdSQLClause )
                            , ( "lastMaxId", lastMaxId )
                            ]
                            template

                    replace entityType ids =
                        let
                            entityIdClause =
                                if ids == [] then
                                    "1=1"
                                else
                                    "event #>> '{data, entityId}' IN (" ++ (String.join ", " <| quoteList ids) ++ ")"
                        in
                            templateReplace [ ( entityType ++ "-entityIds", entityIdClause ) ]

                    sqlWithEntityIds =
                        List.foldl (\( type', ids ) template -> replace type' ids template) sqlTemplate (sndMap Set.toList <| Dict.toList entityIds)

                    sql =
                        RegexE.replace All "\\{\\{.+?\\-entityIds\\}\\}" (RegexE.simpleReplacer "1!=1") sqlWithEntityIds

                    ll =
                        DebugF.log "sql" sql
                in
                    ( updateQueryState model { queryState | currentTemplate = queryState.currentTemplate + 1 }
                    , Postgres.query connectionId sql queryBatchSize (QueryError queryStateId) (Events queryStateId)
                    )
            )
            maybeTemplate
        )
            // ( updateQueryState model { queryState | first = False }, Cmd.none )


processEvents : Model msg -> Int -> List String -> ( Model msg, List msg )
processEvents model queryStateId eventStrs =
    let
        queryState =
            getQueryState queryStateId model

        eventNotInDict =
            "Event not in message dictionary: " ++ (toString queryState.messageDict)

        missingReferenceValue event =
            "Event referenceId is missing: " ++ (toString event)

        eventError eventStr msgs error =
            ( { queryState | badQueryState = True }, queryState.eventProcessingErrorMsg ( eventStr, error ) :: msgs )

        ( newQueryState, msgs ) =
            List.foldl
                (\eventStr ( queryState, msgs ) ->
                    let
                        eventRecordDecoded =
                            decodeString eventRecordDecoder eventStr
                    in
                        Result.map
                            (\eventRecord ->
                                let
                                    event =
                                        eventRecord.event

                                    resultRecordId =
                                        toInt eventRecord.id
                                in
                                    Result.map
                                        (\recordId ->
                                            let
                                                maybeMsg =
                                                    Dict.get event.name queryState.messageDict
                                            in
                                                Maybe.map
                                                    (\{ msg, maybeEntityType } ->
                                                        let
                                                            entityType =
                                                                maybeEntityType // ""

                                                            currentEntityMaxId =
                                                                (Dict.get entityType queryState.maxIds) // -1

                                                            entityMaxId =
                                                                max currentEntityMaxId recordId

                                                            ids =
                                                                Dict.get entityType queryState.ids // Set.empty

                                                            firstTemplateWithDataMaxId =
                                                                max queryState.firstTemplateWithDataMaxId ((Result.toMaybe <| toInt <| eventRecord.max // "-1") // -1)

                                                            updateQueryState maybeIds entityMaxId =
                                                                ( { queryState
                                                                    | firstTemplateWithDataMaxId = firstTemplateWithDataMaxId
                                                                    , ids =
                                                                        maybeIds // queryState.ids
                                                                    , maxIds = Dict.insert entityType entityMaxId queryState.maxIds
                                                                  }
                                                                , (msg eventRecord) :: msgs
                                                                )
                                                        in
                                                            {- add referenced entities to ids dictionary for next SQL query -}
                                                            if not <| isNothing maybeEntityType then
                                                                if isNothing event.data.referenceId then
                                                                    eventError eventStr msgs <| missingReferenceValue eventRecord
                                                                else
                                                                    updateQueryState
                                                                        (Just <| Dict.insert entityType (Set.insert (event.data.referenceId // "") ids) queryState.ids)
                                                                        entityMaxId
                                                            else
                                                                updateQueryState Nothing entityMaxId
                                                    )
                                                    maybeMsg
                                                    // eventError eventStr msgs eventNotInDict
                                        )
                                        resultRecordId
                                        /// (\_ -> eventError eventStr msgs "Corrupt Event Record -- Invalid Id")
                            )
                            eventRecordDecoded
                            /// (\decodingErr -> eventError eventStr msgs decodingErr)
                )
                ( queryState, [] )
                eventStrs
    in
        ( { model | queryStates = Dict.insert queryStateId newQueryState model.queryStates }, List.reverse msgs )


nextQuery : Int -> Int -> Cmd Msg
nextQuery queryStateId connectionId =
    Postgres.moreQueryResults connectionId (QueryError queryStateId) (Events queryStateId)


connectToDb : Model msg -> Int -> (Msg -> msg) -> Cmd msg
connectToDb model queryStateId tagger =
    Postgres.connect model.host model.port' model.database model.user model.password (tagger << (ConnectError queryStateId)) (tagger << (Connect queryStateId)) (tagger << (ConnectionLost queryStateId))



-- Public API


executeQuery : ErrorMsg msg -> EventProcessingErrorMsg msg -> (Int -> msg) -> (Msg -> msg) -> Model msg -> Maybe String -> Query msg -> List String -> Result (List String) ( Model msg, Cmd msg, Int )
executeQuery errorMsg eventProcessingErrorMsg completionMsg tagger model additionalCriteria query rootIds =
    let
        templateResult =
            buildQueryTemplate query
    in
        Result.map
            (\templates ->
                let
                    queryStateId =
                        model.nextId

                    rootEntity =
                        case query of
                            Node nodeQuery _ ->
                                nodeQuery.schema.type'

                            Leaf nodeQuery ->
                                nodeQuery.schema.type'

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
                        , errorMsg = errorMsg
                        , eventProcessingErrorMsg = eventProcessingErrorMsg
                        , completionMsg = completionMsg
                        , tagger = tagger
                        , messageDict = buildMessageDict query
                        , first = True
                        }

                    cmd =
                        connectToDb model queryStateId tagger
                in
                    ( { model | nextId = model.nextId + 1, queryStates = Dict.insert queryStateId queryState model.queryStates }, cmd, queryStateId )
            )
            templateResult


refreshQuery : (Msg -> msg) -> Model msg -> Int -> ( Model msg, Cmd msg )
refreshQuery tagger model queryStateId =
    let
        queryState =
            getQueryState queryStateId model

        newQueryState =
            { queryState | currentTemplate = 0, firstTemplateWithDataMaxId = -1 }

        newModel =
            { model | queryStates = Dict.insert queryStateId newQueryState model.queryStates }
    in
        ( newModel, connectToDb model queryStateId tagger )


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
            , ( "ids", Json.encDict JE.string (JE.list << List.map JE.string << Set.toList) queryState.ids )
            , ( "additionalCriteria", Json.encMaybe JE.string queryState.additionalCriteria )
            , ( "maxIds", Json.encDict JE.string JE.int queryState.maxIds )
            , ( "firstTemplateWithDataMaxId", JE.int queryState.firstTemplateWithDataMaxId )
            ]


exportQueryState : Model msg -> Int -> String
exportQueryState model queryStateId =
    let
        maybeQueryState =
            Dict.get queryStateId model.queryStates
    in
        Maybe.map (\queryState -> queryStateEncode queryState) maybeQueryState // ""


queryStateDecode : Model msg -> ErrorMsg msg -> EventProcessingErrorMsg msg -> (Int -> msg) -> (Msg -> msg) -> MessageDict msg -> String -> Result String (QueryState msg)
queryStateDecode model errorMsg eventProcessingErrorMsg completionMsg tagger messageDict json =
    JD.decodeString
        ((JD.succeed QueryState)
            <|| ("first" := JD.bool)
            <|| ("rootEntity" := JD.string)
            <|| ("badQueryState" := JD.bool)
            <|| ("currentTemplate" := JD.int)
            <|| ("templates" := JD.list JD.string)
            <|| ("rootIds" := JD.list JD.string)
            <|| ("ids" := Json.decConvertDict Set.fromList JD.string (JD.list JD.string))
            <|| ("additionalCriteria" := JD.maybe JD.string)
            <|| ("maxIds" := Json.decDict JD.string JD.int)
            <|| ("firstTemplateWithDataMaxId" := JD.int)
            <|| JD.succeed errorMsg
            <|| JD.succeed eventProcessingErrorMsg
            <|| JD.succeed completionMsg
            <|| JD.succeed tagger
            <|| JD.succeed messageDict
        )
        json


importQueryState : ErrorMsg msg -> EventProcessingErrorMsg msg -> (Int -> msg) -> (Msg -> msg) -> Query msg -> Model msg -> String -> Result String (Model msg)
importQueryState errorMsg eventProcessingErrorMsg completionMsg tagger query model json =
    let
        templateResult =
            buildQueryTemplate query
    in
        Result.map
            (\queryState ->
                let
                    queryStateId =
                        model.nextId
                in
                    { model | nextId = model.nextId + 1, queryStates = Dict.insert queryStateId queryState model.queryStates }
            )
            (queryStateDecode model errorMsg eventProcessingErrorMsg completionMsg tagger (buildMessageDict query) json)
