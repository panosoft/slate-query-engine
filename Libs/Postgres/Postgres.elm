effect module Postgres.Postgres where { command = MyCmd } exposing (..)

import Task exposing (Task)
import Dict exposing (Dict)
import List.Extra as LE exposing (..)
import Native.Postgres


{-| Native client structure
-}
type Client
    = Client


type alias Connection msg =
    { tagger : ConnectTagger msg
    , errorTagger : ErrorTagger msg
    , client : Maybe Client
    }


type alias State msg =
    { nextId : Int
    , connections : Dict Int (Connection msg)
    }


type alias ErrorTagger msg =
    ( Int, String ) -> msg


type alias ConnectTagger msg =
    Int -> msg


type MyCmd msg
    = Connect String Int String String String (ErrorTagger msg) (ConnectTagger msg)
    | Disconnect Int Bool (ErrorTagger msg) (ConnectTagger msg)


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



-- commands


connectionTimeout : Int
connectionTimeout =
    15000


connect : String -> Int -> String -> String -> String -> ErrorTagger msg -> ConnectTagger msg -> Cmd msg
connect host port' database user password errorTagger tagger =
    command (Connect host port' database user password errorTagger tagger)


disconnect : Int -> Bool -> ErrorTagger msg -> ConnectTagger msg -> Cmd msg
disconnect connectionId discardConnection errorTagger tagger =
    command (Disconnect connectionId discardConnection errorTagger tagger)


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
    in
        case cmd of
            Connect host port' database user password errorTagger tagger ->
                let
                    connectionId =
                        state.nextId
                in
                    Native.Postgres.connect (settings1 (ErrorConnect connectionId) (SuccessConnect connectionId)) connectionTimeout host port' database user password
                        &> Task.succeed { state | nextId = state.nextId + 1, connections = Dict.insert connectionId { tagger = tagger, errorTagger = errorTagger, client = Nothing } state.connections }

            Disconnect connectionId discardConnection errorTagger tagger ->
                let
                    maybeConnection =
                        Dict.get connectionId state.connections
                in
                    case maybeConnection of
                        Just connection ->
                            Native.Postgres.disconnect (settings0 (ErrorDisconnect connectionId) (SuccessDisconnect connectionId)) connection.client discardConnection
                                &> Task.succeed { state | connections = Dict.insert connectionId { connection | tagger = tagger, errorTagger = errorTagger } state.connections }

                        Nothing ->
                            (Platform.sendToApp router <| errorTagger ( connectionId, "Invalid connectionId" ))
                                &> Task.succeed state


type Msg
    = SuccessConnect Int Client
    | ErrorConnect Int String
    | SuccessDisconnect Int
    | ErrorDisconnect Int String


crash : Int -> State msg -> Task Never (State msg)
crash id state =
    let
        crash =
            Debug.crash <| "Connection Id: " ++ (toString id) ++ " is not in state: " ++ (toString state)
    in
        Task.succeed state


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router selfMsg state =
    case selfMsg of
        SuccessConnect id client ->
            let
                stateConnection =
                    Dict.get id state.connections
            in
                case stateConnection of
                    Just stateConnection ->
                        let
                            connection =
                                { stateConnection | client = Just client }
                        in
                            Platform.sendToApp router (connection.tagger id) &> Task.succeed { state | nextId = state.nextId + 1, connections = Dict.insert id connection state.connections }

                    Nothing ->
                        crash id state

        ErrorConnect id err ->
            let
                stateConnection =
                    Dict.get id state.connections
            in
                case stateConnection of
                    Just stateConnection ->
                        Platform.sendToApp router (stateConnection.errorTagger ( id, err )) &> Task.succeed { state | connections = Dict.remove id state.connections }

                    Nothing ->
                        crash id state

        SuccessDisconnect id ->
            let
                stateConnection =
                    Dict.get id state.connections
            in
                case stateConnection of
                    Just stateConnection ->
                        Platform.sendToApp router (stateConnection.tagger id) &> Task.succeed { state | connections = Dict.remove id state.connections }

                    Nothing ->
                        crash id state

        ErrorDisconnect id err ->
            let
                stateConnection =
                    Dict.get id state.connections
            in
                case stateConnection of
                    Just stateConnection ->
                        Platform.sendToApp router (stateConnection.errorTagger ( id, err )) &> Task.succeed state

                    Nothing ->
                        crash id state
