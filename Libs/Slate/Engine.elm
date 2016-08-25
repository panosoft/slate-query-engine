module Slate.Engine exposing (..)

import Dict exposing (..)
import Slate.Query exposing (buildQueryTemplate, Query, MessageDict, AppEventMsg)
import Slate.Event exposing (..)
import Postgres.Postgres as Postgres exposing (..)


type alias ErrorMsg msg =
    ( Int, String ) -> msg


type alias CompletionMsg msg =
    Result ( Int, String ) (List Event) -> msg


type alias QueryState msg =
    { currentTemplate : Int
    , templates : List String
    , maxIds : Dict String Int
    , errorMsg : ErrorMsg msg
    , completionMsg : CompletionMsg msg
    , tagger : Msg -> msg
    , messageDict : Slate.Query.MessageDict msg
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
    let
        queryState =
            Dict.get queryStateId model.queryStates
    in
        case queryState of
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
                cmd =
                    startQuery queryStateId connectionId
            in
                ( model ! [ cmd ], [] )

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

        Events queryStateId ( connectionId, list ) ->
            let
                l =
                    Debug.log "Event" list

                cmd =
                    if list == [] then
                        Cmd.none
                    else
                        nextQuery queryStateId connectionId
            in
                ( model ! [ cmd ], [] )

        QueryError queryStateId ( connectionId, error ) ->
            let
                l =
                    Debug.log "QueryError" ( queryStateId, connectionId, error )

                queryState =
                    getQueryState queryStateId model
            in
                ( model ! [], [ queryState.errorMsg <| ( queryStateId, error ) ] )



-- TODO write this (called when connection is complete)


startQuery : Int -> Int -> Cmd Msg
startQuery queryStateId connectionId =
    Postgres.startQuery connectionId "SELECT id FROM events" 10 (QueryError queryStateId) (Events queryStateId)


nextQuery : Int -> Int -> Cmd Msg
nextQuery queryStateId connectionId =
    Postgres.nextQuery connectionId (QueryError queryStateId) (Events queryStateId)



-- TODO
-- closeQUery : Int -> Result String ()


executeQuery : Query msg -> ErrorMsg msg -> CompletionMsg msg -> Model msg -> (Msg -> msg) -> Result (List String) ( Model msg, Cmd msg )
executeQuery query errorMsg completionMsg model tagger =
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
                        { currentTemplate = 0
                        , templates = templates
                        , maxIds = Dict.empty
                        , errorMsg = errorMsg
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
