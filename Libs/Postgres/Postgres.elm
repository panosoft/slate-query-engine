effect module Postgres.Postgres where { command = MyCmd, subscription = MySub } exposing (..)

import String exposing (..)
import Task exposing (Task)
import Dict exposing (Dict)
import Native.Postgres


-- TODO handle highwatermark and batchsize


{-| Native client structure
-}
type Client
    = Client


type alias Connection msg =
    { disconnectionTagger : Maybe (ConnectTagger msg)
    , connectionTagger : Maybe (ConnectTagger msg)
    , queryTagger : Maybe (QueryTagger msg)
    , executeTagger : Maybe (ExecuteTagger msg)
    , listenTagger : Maybe (ListenTagger msg)
    , errorTagger : ErrorTagger msg
    , eventTagger : Maybe (ListenEventTagger msg)
    , client : Maybe Client
    , stream : Maybe Stream
    , recordCount : Maybe Int
    , sql : Maybe String
    }


type NativeListener
    = NativeListener


type alias ListenerState msg =
    { channel : String
    , errorTagger : ErrorTagger msg
    , listenTagger : ListenTagger msg
    , eventTagger : ListenEventTagger msg
    }


type alias ListenerDict msg =
    Dict Int (ListenerState msg)


type alias NativeListenerDict =
    Dict Int NativeListener


type alias State msg =
    { nextId : Int
    , connections : Dict Int (Connection msg)
    , listeners : ListenerDict msg
    , nativeListeners : NativeListenerDict
    }


type alias ErrorTagger msg =
    ( Int, String ) -> msg


type alias ConnectTagger msg =
    Int -> msg


type alias DisconnectTagger msg =
    Int -> msg


type alias QueryTagger msg =
    ( Int, List String ) -> msg


type alias ExecuteTagger msg =
    ( Int, Int ) -> msg


type MyCmd msg
    = Connect String Int String String String (ErrorTagger msg) (ConnectTagger msg)
    | Disconnect Int Bool (ErrorTagger msg) (DisconnectTagger msg)
    | Query Int String Int (ErrorTagger msg) (QueryTagger msg)
    | MoreQueryResults Int (ErrorTagger msg) (QueryTagger msg)
    | ExecuteSQL Int String (ErrorTagger msg) (ExecuteTagger msg)


(//) : Maybe a -> a -> a
(//) =
    flip Maybe.withDefault


init : Task Never (State msg)
init =
    Task.succeed (State 0 Dict.empty Dict.empty Dict.empty)


cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap f cmd =
    case cmd of
        Connect host port' database user password errorTagger tagger ->
            Connect host port' database user password (f << errorTagger) (f << tagger)

        Disconnect client discardConnection errorTagger tagger ->
            Disconnect client discardConnection (f << errorTagger) (f << tagger)

        Query connectionId sql recordCount errorTagger tagger ->
            Query connectionId sql recordCount (f << errorTagger) (f << tagger)

        MoreQueryResults connectionId errorTagger tagger ->
            MoreQueryResults connectionId (f << errorTagger) (f << tagger)

        ExecuteSQL connectionId sql errorTagger tagger ->
            ExecuteSQL connectionId sql (f << errorTagger) (f << tagger)



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


query : Int -> String -> Int -> ErrorTagger msg -> QueryTagger msg -> Cmd msg
query connectionId sql recordCount errorTagger tagger =
    command (Query connectionId sql recordCount errorTagger tagger)


moreQueryResults : Int -> ErrorTagger msg -> QueryTagger msg -> Cmd msg
moreQueryResults connectionId errorTagger tagger =
    command (MoreQueryResults connectionId errorTagger tagger)


executeSQL : Int -> String -> ErrorTagger msg -> ExecuteTagger msg -> Cmd msg
executeSQL connectionId sql errorTagger tagger =
    command (ExecuteSQL connectionId sql errorTagger tagger)



-- subscriptions


type alias ListenTagger msg =
    ( Int, String, String ) -> msg


type alias ListenEventTagger msg =
    ( Int, String, String ) -> msg


type MySub msg
    = Listen Int String (ErrorTagger msg) (ListenTagger msg) (ListenEventTagger msg)


subMap : (a -> b) -> MySub a -> MySub b
subMap f sub =
    case sub of
        Listen connectionId channel errorTagger listenTagger eventTagger ->
            Listen connectionId channel (f << errorTagger) (f << listenTagger) (f << eventTagger)


listen : Int -> String -> ErrorTagger msg -> ListenTagger msg -> ListenEventTagger msg -> Sub msg
listen connectionId channel errorTagger listenTagger eventTagger =
    subscription (Listen connectionId channel errorTagger listenTagger eventTagger)


(&>) : Task x a -> Task x b -> Task x b
(&>) t1 t2 =
    t1 `Task.andThen` \_ -> t2


(&>>) : Task x a -> (a -> Task x b) -> Task x b
(&>>) t1 f =
    t1 `Task.andThen` f


onEffects : Platform.Router msg Msg -> List (MyCmd msg) -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router cmds newSubs state =
    let
        newSubsDict =
            List.foldl addMySub Dict.empty newSubs

        oldListeners =
            Dict.diff state.listeners newSubsDict

        newListeners =
            Dict.diff newSubsDict state.listeners

        keepListeners =
            Dict.intersect state.listeners newSubsDict

        handleOneCmd state cmd tasks =
            let
                ( task, newState ) =
                    handleCmd router state cmd
            in
                ( task :: tasks, newState )

        ( tasks, cmdState ) =
            List.foldl (\cmd ( tasks, state ) -> handleOneCmd state cmd tasks) ( [], state ) cmds

        cmdTask =
            Task.sequence (List.reverse tasks)

        ( stopTask, stopState ) =
            stopListeners router oldListeners cmdState

        ( startTask, startState ) =
            startListeners router newListeners stopState
    in
        cmdTask
            &> stopTask
            &> startTask
            &> Task.succeed { startState | listeners = Dict.union keepListeners newListeners }


startStopListeners : String -> Platform.Router msg Msg -> ListenerDict msg -> State msg -> ( Task Never (), State msg )
startStopListeners listenUnlisten router listeners state =
    let
        startStopListener connectionId listenerState ( task, state ) =
            let
                ( executeTask, executeState ) =
                    let
                        sql =
                            listenUnlisten ++ " \"" ++ listenerState.channel ++ "\""

                        nativeListener =
                            Dict.get connectionId state.nativeListeners

                        getTask connection type' =
                            let
                                settings =
                                    (settings1 router (ErrorExecuteSQL connectionId sql) (SuccessListenUnlisten listenerState.channel type' connectionId))
                            in
                                if type' == "listen" then
                                    Native.Postgres.listen settings connection.client sql (\message -> Platform.sendToSelf router (ListenEvent connectionId listenerState.channel message))
                                else
                                    Native.Postgres.unlisten settings connection.client sql nativeListener

                        maybeTask =
                            Maybe.map
                                (\connection ->
                                    ( getTask connection listenUnlisten
                                    , updateConnection state
                                        connectionId
                                        { connection
                                            | sql = Just sql
                                            , listenTagger = Just listenerState.listenTagger
                                            , errorTagger = listenerState.errorTagger
                                            , eventTagger = Just listenerState.eventTagger
                                        }
                                    )
                                )
                                (Dict.get connectionId state.connections)
                    in
                        maybeTask
                            // ( invalidConnectionId router listenerState.errorTagger connectionId, state )
            in
                ( executeTask &> task, executeState )
    in
        Dict.foldl startStopListener ( Task.succeed (), state ) listeners


stopListeners : Platform.Router msg Msg -> ListenerDict msg -> State msg -> ( Task Never (), State msg )
stopListeners =
    startStopListeners "unlisten"


startListeners : Platform.Router msg Msg -> ListenerDict msg -> State msg -> ( Task Never (), State msg )
startListeners =
    startStopListeners "listen"


addMySub : MySub msg -> ListenerDict msg -> ListenerDict msg
addMySub sub dict =
    case sub of
        Listen connectionId channel errorTagger listenTagger eventTagger ->
            Dict.insert connectionId (ListenerState channel errorTagger listenTagger eventTagger) dict


updateConnection : State msg -> Int -> Connection msg -> State msg
updateConnection state connectionId newConnection =
    { state | connections = Dict.insert connectionId newConnection state.connections }


settings0 : Platform.Router msg Msg -> (a -> Msg) -> Msg -> { onError : a -> Task msg (), onSuccess : Never -> Task msg () }
settings0 router errorTagger tagger =
    { onError = \err -> Platform.sendToSelf router (errorTagger err)
    , onSuccess = \_ -> Platform.sendToSelf router tagger
    }


settings1 : Platform.Router msg Msg -> (a -> Msg) -> (b -> Msg) -> { onError : a -> Task Never (), onSuccess : b -> Task msg () }
settings1 router errorTagger tagger =
    { onError = \err -> Platform.sendToSelf router (errorTagger err)
    , onSuccess = \result1 -> Platform.sendToSelf router (tagger result1)
    }


settings2 : Platform.Router msg Msg -> (a -> Msg) -> (b -> c -> Msg) -> { onError : a -> Task Never (), onSuccess : b -> c -> Task msg () }
settings2 router errorTagger tagger =
    { onError = \err -> Platform.sendToSelf router (errorTagger err)
    , onSuccess = \result1 result2 -> Platform.sendToSelf router (tagger result1 result2)
    }


invalidConnectionId : Platform.Router msg Msg -> ErrorTagger msg -> Int -> Task Never ()
invalidConnectionId router errorTagger connectionId =
    Platform.sendToApp router <| errorTagger ( connectionId, "Invalid connectionId" )


handleCmd : Platform.Router msg Msg -> State msg -> MyCmd msg -> ( Task Never (), State msg )
handleCmd router state cmd =
    case cmd of
        Connect host port' database user password errorTagger tagger ->
            let
                connectionId =
                    state.nextId

                newConnection =
                    Connection Nothing (Just tagger) Nothing Nothing Nothing errorTagger Nothing Nothing Nothing Nothing Nothing
            in
                ( Native.Postgres.connect (settings1 router (ErrorConnect connectionId) (SuccessConnect connectionId)) connectionTimeout host port' database user password
                , { state | nextId = state.nextId + 1, connections = Dict.insert connectionId newConnection state.connections }
                )

        Disconnect connectionId discardConnection errorTagger tagger ->
            let
                maybeTask =
                    Maybe.map
                        (\connection ->
                            ( Native.Postgres.disconnect (settings0 router (ErrorDisconnect connectionId) (SuccessDisconnect connectionId)) connection.client discardConnection
                            , updateConnection state connectionId { connection | disconnectionTagger = Just tagger, errorTagger = errorTagger }
                            )
                        )
                        (Dict.get connectionId state.connections)
            in
                maybeTask // ( invalidConnectionId router errorTagger connectionId, state )

        Query connectionId sql recordCount errorTagger tagger ->
            let
                maybeTask =
                    Maybe.map
                        (\connection ->
                            ( Native.Postgres.query (settings2 router (ErrorQuery connectionId sql) (SuccessQuery connectionId)) connection.client sql recordCount
                            , updateConnection state connectionId { connection | sql = Just sql, recordCount = Just recordCount, queryTagger = Just tagger, errorTagger = errorTagger }
                            )
                        )
                        (Dict.get connectionId state.connections)
            in
                maybeTask // ( invalidConnectionId router errorTagger connectionId, state )

        MoreQueryResults connectionId errorTagger tagger ->
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
                                ( Native.Postgres.moreQueryResults (settings2 router (ErrorQuery connectionId sql) (SuccessQuery connectionId)) connection.client stream recordCount
                                , state
                                )
                        )
                    <|
                        Dict.get connectionId state.connections
            in
                maybeTask // ( invalidConnectionId router errorTagger connectionId, state )

        ExecuteSQL connectionId sql errorTagger tagger ->
            let
                maybeTask =
                    Maybe.map
                        (\connection ->
                            ( Native.Postgres.executeSQL (settings1 router (ErrorExecuteSQL connectionId sql) (SuccessExecuteSQL connectionId)) connection.client sql
                            , updateConnection state connectionId { connection | sql = Just sql, executeTagger = Just tagger, errorTagger = errorTagger }
                            )
                        )
                        (Dict.get connectionId state.connections)
            in
                maybeTask // ( invalidConnectionId router errorTagger connectionId, state )


type Stream
    = Stream


type Msg
    = SuccessConnect Int Client
    | ErrorConnect Int String
    | SuccessDisconnect Int
    | ErrorDisconnect Int String
    | SuccessQuery Int Stream (List String)
    | ErrorQuery Int String String
    | SuccessExecuteSQL Int Int
    | ErrorExecuteSQL Int String String
    | SuccessListenUnlisten String String Int NativeListener
    | ErrorListenUnlisten String String Int String String
    | ListenEvent Int String String


printableConnection : Connection msg -> Connection msg
printableConnection connection =
    { connection | client = Nothing, stream = Nothing }


printableState : State msg -> State msg
printableState state =
    { state | connections = Dict.map (\_ connection -> printableConnection connection) state.connections }


crash : State msg -> String -> Task Never (State msg)
crash state msg =
    let
        crash =
            Debug.crash msg
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
                crash state <| "Connection Id: " ++ (toString connectionId) ++ " is not in state: " ++ (toString <| printableState state)


withTagger : State msg -> Maybe tagger -> String -> (tagger -> Task Never (State msg)) -> Task Never (State msg)
withTagger state maybeTagger type' f =
    case maybeTagger of
        Just tagger ->
            f tagger

        Nothing ->
            crash state <| "Missing " ++ type' ++ " Tagger in state: " ++ (toString <| printableState state)


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router selfMsg state =
    let
        sqlError connectionId sql err =
            let
                process connection =
                    Platform.sendToApp router (connection.errorTagger ( connectionId, err ++ "\n\nCommand:\n" ++ sql ))
                        &> Task.succeed state
            in
                withConnection state connectionId process
    in
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
                            withTagger state newConnection.connectionTagger "Connect" sendToApp
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
                            withTagger state connection.disconnectionTagger "Disconnect" sendToApp
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
                            withTagger state connection.queryTagger "Query" sendToApp
                in
                    withConnection state connectionId process

            ErrorQuery connectionId sql err ->
                sqlError connectionId sql err

            SuccessExecuteSQL connectionId result ->
                let
                    process connection =
                        let
                            sendToApp tagger =
                                Platform.sendToApp router (tagger ( connectionId, result ))
                                    &> Task.succeed state
                        in
                            withTagger state connection.executeTagger "Execute" sendToApp
                in
                    withConnection state connectionId process

            ErrorExecuteSQL connectionId sql err ->
                sqlError connectionId sql err

            SuccessListenUnlisten channel type' connectionId nativeListener ->
                let
                    newListener =
                        (Maybe.map (\listenerState -> listenerState) (Dict.get connectionId state.listeners))

                    process connection =
                        let
                            newState =
                                if type' == "listen" then
                                    { state | nativeListeners = Dict.insert connectionId nativeListener state.nativeListeners }
                                else
                                    state

                            sendToApp tagger =
                                Platform.sendToApp router (tagger ( connectionId, channel, type' ))
                                    &> Task.succeed newState
                        in
                            withTagger state connection.listenTagger "ListenUnlisten" sendToApp
                in
                    withConnection state connectionId process

            ErrorListenUnlisten channel type' connectionId sql err ->
                sqlError connectionId sql (String.join "," [ channel, type', err ])

            ListenEvent connectionId channel message ->
                let
                    process connection =
                        let
                            sendToApp tagger =
                                Platform.sendToApp router (tagger ( connectionId, channel, message ))
                                    &> Task.succeed state
                        in
                            withTagger state connection.eventTagger "ListenEvent" sendToApp
                in
                    withConnection state connectionId process
