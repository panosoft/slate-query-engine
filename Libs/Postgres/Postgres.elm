effect module Postgres.Postgres where { command = MyCmd } exposing (..)

import Task exposing (Task)
import Dict exposing (Dict)
import List.Extra as LE exposing (..)
import Native.Postgres


-- TODO handle highwatermark and batchsize


{-| Native client structure
-}
type Client
    = Client


type alias Connection msg =
    { connectionTagger : Maybe (ConnectTagger msg)
    , queryTagger : Maybe (QueryTagger msg)
    , errorTagger : ErrorTagger msg
    , client : Maybe Client
    , stream : Maybe Stream
    , recordCount : Maybe Int
    , sql : Maybe String
    }


type alias State msg =
    { nextId : Int
    , connections : Dict Int (Connection msg)
    }


type alias ErrorTagger msg =
    ( Int, String ) -> msg


type alias ConnectTagger msg =
    Int -> msg


type alias DisconnectTagger msg =
    Int -> msg


type alias QueryTagger msg =
    ( Int, List String ) -> msg


type MyCmd msg
    = Connect String Int String String String (ErrorTagger msg) (ConnectTagger msg)
    | Disconnect Int Bool (ErrorTagger msg) (DisconnectTagger msg)
    | StartQuery Int String Int (ErrorTagger msg) (QueryTagger msg)
    | NextQuery Int (ErrorTagger msg) (QueryTagger msg)


(//) : Maybe a -> a -> a
(//) =
    flip Maybe.withDefault


init : Task Never (State msg)
init =
    Task.succeed (State 0 Dict.empty)


cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap f cmd =
    case cmd of
        Connect host port' database user password errorTagger tagger ->
            Connect host port' database user password (f << errorTagger) (f << tagger)

        Disconnect client discardConnection errorTagger tagger ->
            Disconnect client discardConnection (f << errorTagger) (f << tagger)

        StartQuery connectionId sql recordCount errorTagger tagger ->
            StartQuery connectionId sql recordCount (f << errorTagger) (f << tagger)

        NextQuery connectionId errorTagger tagger ->
            NextQuery connectionId (f << errorTagger) (f << tagger)



-- commands


connectionTimeout : Int
connectionTimeout =
    15000


connect : String -> Int -> String -> String -> String -> ErrorTagger msg -> ConnectTagger msg -> Cmd msg
connect host port' database user password errorTagger tagger =
    command (Connect host port' database user password errorTagger tagger)


disconnect : Int -> Bool -> ErrorTagger msg -> DisconnectTagger msg -> Cmd msg
disconnect connectionId discardConnection errorTagger tagger =
    command (Disconnect connectionId discardConnection errorTagger tagger)


startQuery : Int -> String -> Int -> ErrorTagger msg -> QueryTagger msg -> Cmd msg
startQuery connectionId sql recordCount errorTagger tagger =
    command (StartQuery connectionId sql recordCount errorTagger tagger)


nextQuery : Int -> ErrorTagger msg -> QueryTagger msg -> Cmd msg
nextQuery connectionId errorTagger tagger =
    command (NextQuery connectionId errorTagger tagger)


(&>) : Task x a -> Task x b -> Task x b
(&>) t1 t2 =
    t1 `Task.andThen` \_ -> t2


(&>>) : Task x a -> (a -> Task x b) -> Task x b
(&>>) t1 f =
    t1 `Task.andThen` f


onEffects : Platform.Router msg Msg -> List (MyCmd msg) -> State msg -> Task Never (State msg)
onEffects router cmds state =
    (LE.foldl1 (&>) <| List.map (handleCmd router state) cmds) // Task.succeed state


handleCmd : Platform.Router msg Msg -> State msg -> MyCmd msg -> Task Never (State msg)
handleCmd router state cmd =
    let
        settings0 errorTagger tagger =
            { onSuccess = \_ -> Platform.sendToSelf router tagger
            , onError = \err -> Platform.sendToSelf router (errorTagger err)
            }

        settings1 errorTagger tagger =
            { onSuccess = \result1 -> Platform.sendToSelf router (tagger result1)
            , onError = \err -> Platform.sendToSelf router (errorTagger err)
            }

        settings2 errorTagger tagger =
            { onSuccess = \result1 result2 -> Platform.sendToSelf router (tagger result1 result2)
            , onError = \err -> Platform.sendToSelf router (errorTagger err)
            }

        invalidConnectionId errorTagger connectionId =
            (Platform.sendToApp router <| errorTagger ( connectionId, "Invalid connectionId" ))
                &> Task.succeed state

        updateConnection connectionId newConnection =
            Task.succeed
                { state | connections = Dict.insert connectionId newConnection state.connections }

        toTask maybeTask connectionId errorTagger =
            case maybeTask of
                Just task ->
                    task

                Nothing ->
                    invalidConnectionId errorTagger connectionId
    in
        case cmd of
            Connect host port' database user password errorTagger tagger ->
                let
                    connectionId =
                        state.nextId

                    connection =
                        Connection (Just tagger) Nothing errorTagger Nothing Nothing Nothing Nothing
                in
                    Native.Postgres.connect (settings1 (ErrorConnect connectionId) (SuccessConnect connectionId)) connectionTimeout host port' database user password
                        &> Task.succeed
                            { state
                                | nextId = state.nextId + 1
                                , connections = Dict.insert connectionId connection state.connections
                            }

            Disconnect connectionId discardConnection errorTagger tagger ->
                let
                    maybeTask =
                        Maybe.map
                            (\connection ->
                                Native.Postgres.disconnect (settings0 (ErrorDisconnect connectionId) (SuccessDisconnect connectionId)) connection.client discardConnection
                                    &> updateConnection connectionId { connection | connectionTagger = Just tagger, errorTagger = errorTagger }
                            )
                        <|
                            Dict.get connectionId state.connections
                in
                    toTask maybeTask connectionId errorTagger

            StartQuery connectionId sql recordCount errorTagger tagger ->
                let
                    maybeTask =
                        Maybe.map
                            (\connection ->
                                Native.Postgres.startQuery (settings2 (ErrorQuery connectionId sql) (SuccessQuery connectionId)) connection.client sql recordCount
                                    &> updateConnection connectionId { connection | sql = Just sql, recordCount = Just recordCount, queryTagger = Just tagger, errorTagger = errorTagger }
                            )
                        <|
                            Dict.get connectionId state.connections
                in
                    toTask maybeTask connectionId errorTagger

            NextQuery connectionId errorTagger tagger ->
                let
                    maybeTask =
                        Maybe.map
                            (\connection ->
                                let
                                    sql =
                                        connection.sql // "UNKNOWN SQL"

                                    recordCount =
                                        case connection.recordCount of
                                            Just recordCount ->
                                                recordCount

                                            Nothing ->
                                                Debug.crash ("Missing Record Count connectionId: " ++ (toString connectionId)) 0

                                    stream =
                                        case connection.stream of
                                            Just stream ->
                                                stream

                                            Nothing ->
                                                Debug.crash ("Missing Stream connectionId: " ++ (toString connectionId)) Stream
                                in
                                    Native.Postgres.nextQuery (settings2 (ErrorQuery connectionId sql) (SuccessQuery connectionId)) connection.client stream recordCount
                                        &> Task.succeed state
                            )
                        <|
                            Dict.get connectionId state.connections
                in
                    toTask maybeTask connectionId errorTagger


type Stream
    = Stream


type Msg
    = SuccessConnect Int Client
    | ErrorConnect Int String
    | SuccessDisconnect Int
    | ErrorDisconnect Int String
    | SuccessQuery Int Stream (List String)
    | ErrorQuery Int String String


crash : Int -> State msg -> Task Never (State msg)
crash id state =
    let
        crash =
            Debug.crash <| "Connection Id: " ++ (toString id) ++ " is not in state: " ++ (toString state)
    in
        Task.succeed state


withConnection : State msg -> Int -> (Connection msg -> Task Never (State msg)) -> Task Never (State msg)
withConnection state connectionId f =
    let
        stateConnection =
            Dict.get connectionId state.connections
    in
        case stateConnection of
            Just stateConnection ->
                f stateConnection

            Nothing ->
                crash connectionId state


withTagger : State msg -> Maybe tagger -> Int -> (tagger -> Task Never (State msg)) -> Task Never (State msg)
withTagger state maybeTagger connectionId f =
    case maybeTagger of
        Just tagger ->
            f tagger

        Nothing ->
            crash connectionId state


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router selfMsg state =
    case selfMsg of
        SuccessConnect connectionId client ->
            let
                process connection =
                    let
                        newConnection =
                            { connection | client = Just client }

                        sendToApp tagger =
                            Platform.sendToApp router (tagger connectionId)
                                &> Task.succeed { state | connections = Dict.insert connectionId newConnection state.connections }
                    in
                        withTagger state newConnection.connectionTagger connectionId sendToApp
            in
                withConnection state connectionId process

        ErrorConnect connectionId err ->
            let
                process connection =
                    Platform.sendToApp router (connection.errorTagger ( connectionId, err ))
                        &> Task.succeed { state | connections = Dict.remove connectionId state.connections }
            in
                withConnection state connectionId process

        SuccessDisconnect connectionId ->
            let
                process connection =
                    let
                        sendToApp tagger =
                            Platform.sendToApp router (tagger connectionId)
                                &> Task.succeed { state | connections = Dict.remove connectionId state.connections }
                    in
                        withTagger state connection.connectionTagger connectionId sendToApp
            in
                withConnection state connectionId process

        ErrorDisconnect connectionId err ->
            let
                process connection =
                    Platform.sendToApp router (connection.errorTagger ( connectionId, err ))
                        &> Task.succeed state
            in
                withConnection state connectionId process

        SuccessQuery connectionId stream results ->
            let
                process connection =
                    let
                        sendToApp tagger =
                            Platform.sendToApp router (tagger ( connectionId, results ))
                                &> Task.succeed { state | connections = Dict.insert connectionId { connection | stream = Just stream } state.connections }
                    in
                        withTagger state connection.queryTagger connectionId sendToApp
            in
                withConnection state connectionId process

        ErrorQuery connectionId sql err ->
            let
                process connection =
                    Platform.sendToApp router (connection.errorTagger ( connectionId, err ++ "\n\nCommand:\n" ++ sql ))
                        &> Task.succeed state
            in
                withConnection state connectionId process
