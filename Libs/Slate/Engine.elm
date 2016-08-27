module Slate.Engine exposing (..)

import String exposing (..)
import Dict exposing (..)
import Set exposing (..)
import Json.Decode exposing (decodeString)
import List.Extra as LE exposing (..)
import Regex exposing (HowMany(All, AtMost))
import Regex.Extra as RE exposing (..)
import Slate.Query exposing (Query(..), MessageDict, MessageDictEntry, AppEventMsg, buildQueryTemplate, parametricReplace)
import Slate.Event exposing (EventRecord, Event, eventRecordDecoder)
import Utils.Utils exposing (..)
import Postgres.Postgres as Postgres exposing (..)


queryBatchSize : Int
queryBatchSize =
    10


type alias ErrorMsg msg =
    ( Int, String ) -> msg


type alias EventProcessingErrorMsg msg =
    ( String, String ) -> msg


type alias QueryState msg =
    { query : Query msg
    , badQueryState : Bool
    , currentTemplate : Int
    , templates : List String
    , rootIds : List String
    , ids : Dict String (Set String)
    , additionalCriteria : Maybe String
    , maxIds : Dict String Int
    , firstQueryMaxId : Int
    , errorMsg : ErrorMsg msg
    , eventProcessingErrorMsg : EventProcessingErrorMsg msg
    , completionMsg : msg
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


update : Msg -> Model msg -> ( ( Model msg, Cmd Msg ), List msg )
update msg model =
    case msg of
        Nop ->
            ( model ! [], [] )

        ConnectError queryStateId ( connectionId, error ) ->
            let
                l =
                    Debug.log "ConnectError" ( queryStateId, connectionId, error )

                queryState =
                    getQueryState queryStateId model
            in
                ( model ! [], [ queryState.errorMsg <| ( queryStateId, error ) ] )

        Connect queryStateId connectionId ->
            let
                l =
                    Debug.log "Connect" ( queryStateId, connectionId )

                queryState =
                    getQueryState queryStateId model

                -- cmd =
                --     Postgres.disconnect connectionId False (DisconnectError queryStateId) (Disconnect queryStateId)
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
                ( model ! [], [ queryState.errorMsg <| ( queryStateId, error ) ] )

        Disconnect queryStateId connectionId ->
            let
                l =
                    Debug.log "Disconnect" ( queryStateId, connectionId )

                queryState =
                    getQueryState queryStateId model
            in
                ( model ! [], [] )

        Events queryStateId ( connectionId, eventStrs ) ->
            let
                l =
                    Debug.log (toString eventStrs) "Event"

                ( updatedModel, msgs ) =
                    processEvents model queryStateId eventStrs

                ( newModel, cmd ) =
                    let
                        queryState =
                            getQueryState queryStateId model
                    in
                        if queryState.badQueryState == False then
                            if eventStrs == [] then
                                startQuery updatedModel queryStateId connectionId
                            else
                                ( updatedModel, nextQuery queryStateId connectionId )
                        else
                            model ! []
            in
                ( newModel ! [ cmd ], msgs )

        QueryError queryStateId ( connectionId, error ) ->
            let
                l =
                    Debug.log "QueryError" ( queryStateId, connectionId, error )

                queryState =
                    getQueryState queryStateId model
            in
                ( model ! [], [ queryState.errorMsg <| ( queryStateId, error ) ] )



-- TODO write this (called when connection is complete)


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

        firstQuery =
            queryState.currentTemplate == 0

        firstQueryMaxCriteria =
            if firstQuery then
                ""
            else
                "AND id < " ++ (toString queryState.firstQueryMaxId)

        entityIds : Dict String (Set String)
        entityIds =
            let
                rootEntity =
                    case queryState.query of
                        Node nodeQuery _ ->
                            nodeQuery.schema.type'

                        Leaf nodeQuery ->
                            nodeQuery.schema.type'
            in
                if firstQuery then
                    Dict.insert rootEntity (Set.fromList queryState.rootIds) Dict.empty
                else
                    queryState.ids

        lastMaxId =
            toString <| (LE.foldl1 max <| Dict.values queryState.maxIds) // -1
    in
        (Maybe.map
            (\template ->
                let
                    sqlTemplate =
                        templateReplace
                            [ ( "additionalCriteria", queryState.additionalCriteria // "1=1" )
                            , ( "firstQueryMaxCriteria", firstQueryMaxCriteria )
                            , ( "lastMaxId", lastMaxId )
                            ]
                            template

                    replace entityType ids =
                        let
                            entityIdClause =
                                if ids == [] then
                                    "1=1"
                                else
                                    "entity_id IN (" ++ (String.join ", " <| quoteList ids) ++ ")"
                        in
                            templateReplace [ ( entityType ++ "-entityIds", entityIdClause ) ]

                    sqlWithEntityIds =
                        List.foldl (\( type', ids ) template -> replace type' ids template) sqlTemplate (sndMap Set.toList <| Dict.toList entityIds)

                    sql =
                        RE.replace All "\\{\\{.+?\\-entityIds\\}\\}" (RE.simpleReplacer "1!=1") sqlWithEntityIds

                    ll =
                        Debug.log sql "sql"
                in
                    ( { model | queryStates = Dict.insert queryStateId { queryState | currentTemplate = queryState.currentTemplate + 1 } model.queryStates }
                    , Postgres.startQuery connectionId sql queryBatchSize (QueryError queryStateId) (Events queryStateId)
                    )
            )
            maybeTemplate
        )
            // ( model, Cmd.none )


(///) : Result err value -> (err -> value) -> value
(///) result f =
    case result of
        Ok value ->
            value

        Err err ->
            f err


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

        l =
            Debug.log "ids:::::::::::" queryState.ids

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

                                    maybeMsg =
                                        Dict.get event.name queryState.messageDict
                                in
                                    Maybe.map
                                        (\{ msg, maybeEntityType } ->
                                            let
                                                entityType =
                                                    maybeEntityType // ""

                                                ids =
                                                    Dict.get entityType queryState.ids // Set.empty

                                                maxId =
                                                    (Result.toMaybe <| toInt <| eventRecord.max // "-1") // -1

                                                firstQueryMaxId =
                                                    max queryState.firstQueryMaxId <| maxId
                                            in
                                                {- add referenced entities to ids dictionary for next SQL query -}
                                                if not <| isNothing maybeEntityType then
                                                    if isNothing event.data.referenceId then
                                                        eventError eventStr msgs <| missingReferenceValue eventRecord
                                                    else
                                                        ( { queryState
                                                            | ids = Dict.insert entityType (Set.insert (event.data.referenceId // "") ids) queryState.ids
                                                            , firstQueryMaxId = firstQueryMaxId
                                                          }
                                                        , (msg eventRecord) :: msgs
                                                        )
                                                else
                                                    ( { queryState | firstQueryMaxId = firstQueryMaxId }, (msg eventRecord) :: msgs )
                                        )
                                        maybeMsg
                                        // eventError eventStr msgs eventNotInDict
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
    Postgres.nextQuery connectionId (QueryError queryStateId) (Events queryStateId)



-- TODO
-- closeQUery : Int -> Result String ()


executeQuery : Query msg -> List String -> Maybe String -> ErrorMsg msg -> EventProcessingErrorMsg msg -> msg -> Model msg -> (Msg -> msg) -> Result (List String) ( Model msg, Cmd msg )
executeQuery query rootIds additionalCriteria errorMsg eventProcessingErrorMsg completionMsg model tagger =
    let
        result =
            buildQueryTemplate query
    in
        case result of
            Ok templates ->
                let
                    queryStateId =
                        model.nextId

                    queryState =
                        { query = query
                        , badQueryState = False
                        , currentTemplate = 0
                        , templates = templates
                        , rootIds = rootIds
                        , ids = Dict.empty
                        , additionalCriteria = additionalCriteria
                        , maxIds = Dict.empty
                        , firstQueryMaxId = -1
                        , errorMsg = errorMsg
                        , eventProcessingErrorMsg = eventProcessingErrorMsg
                        , completionMsg = completionMsg
                        , tagger = tagger
                        , messageDict = Slate.Query.buildMessageDict query
                        }

                    cmd =
                        Postgres.connect model.host model.port' model.database model.user model.password (tagger << (ConnectError queryStateId)) (tagger << (Connect queryStateId))
                in
                    Ok ( { model | nextId = model.nextId + 1, queryStates = Dict.insert queryStateId queryState model.queryStates }, cmd )

            Err errs ->
                Err errs