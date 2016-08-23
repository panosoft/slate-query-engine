module Slate.Engine exposing (..)

import Dict exposing (..)
import Slate.Query exposing (buildQueryTemplate, Query)
import Slate.Event exposing (..)
import Postgres.Postgres as Postgres exposing (..)


type alias ConnectionMsg msg =
    Result ( Int, String ) Int -> msg


type alias DisconnectionMsg msg =
    ConnectionMsg msg


type alias CompletionMsg msg =
    Result ( Int, String ) (List Event) -> msg


type alias QueryState msg =
    { currentTemplate : Int
    , templates : List String
    , maxIds : Dict String Int
    , connectionMsg : ConnectionMsg msg
    , disconnectionMsg : DisconnectionMsg msg
    , completionMsg : CompletionMsg msg
    , tagger : Msg -> msg
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
    | Events Int (List Event)


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


update : Msg -> Model msg -> ( Model msg, Cmd Msg, Maybe msg )
update msg model =
    case msg of
        Nop ->
            ( model, Cmd.none, Nothing )

        ConnectError queryStateId ( connectionId, error ) ->
            let
                l =
                    Debug.log "ConnectError" ( queryStateId, connectionId, error )

                queryState =
                    getQueryState queryStateId model
            in
                ( model, Cmd.none, Just <| queryState.connectionMsg <| Err ( queryStateId, error ) )

        Connect queryStateId connectionId ->
            let
                l =
                    Debug.log "Connect" ( queryStateId, connectionId )

                queryState =
                    getQueryState queryStateId model

                cmd =
                    Postgres.disconnect connectionId False (DisconnectError queryStateId) (Disconnect queryStateId)
            in
                ( model, cmd, Just <| queryState.connectionMsg <| Ok connectionId )

        DisconnectError queryStateId ( connectionId, error ) ->
            let
                l =
                    Debug.log "DisconnectError" ( queryStateId, connectionId, error )

                queryState =
                    getQueryState queryStateId model
            in
                ( model, Cmd.none, Just <| queryState.disconnectionMsg <| Err ( queryStateId, error ) )

        Disconnect queryStateId connectionId ->
            let
                l =
                    Debug.log "Disconnect" ( queryStateId, connectionId )

                queryState =
                    getQueryState queryStateId model
            in
                ( model, Cmd.none, Just <| queryState.disconnectionMsg <| Ok connectionId )

        Events id list ->
            ( model, Cmd.none, Nothing )



-- startQuery : Int -> Int -> Cmd msg
-- startQuery = queryStateId connectionId


executeQuery : Query (Event -> msg) -> ConnectionMsg msg -> DisconnectionMsg msg -> CompletionMsg msg -> Model msg -> (Msg -> msg) -> Result (List String) ( Model msg, Cmd msg )
executeQuery query connectionMsg disconnectionMsg completionMsg model tagger =
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
                        , connectionMsg = connectionMsg
                        , disconnectionMsg = disconnectionMsg
                        , completionMsg = completionMsg
                        , tagger = tagger
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
