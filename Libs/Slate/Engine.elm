module Slate.Engine exposing (..)

import Dict exposing (..)
import Slate.Query exposing (..)
import Slate.Event exposing (..)


type alias QueryState =
    { remainingTemplates : List String
    , maxIds : Dict String Int
    }


type alias Model =
    { nextId : Int
    , queryStates : Dict Int (List QueryState)
    }


initModel : Model
initModel =
    { nextId = 0
    , queryStates = Dict.empty
    }


type Msg
    = Nop
    | Events Int (List Event)


update : Msg -> Model -> ( Model, Cmd Msg, Cmd msg )
update msg model =
    case msg of
        Nop ->
            ( model, Cmd.none, Cmd.none )

        Events id list ->
            ( model, Cmd.none, Cmd.none )


executeQuery : Query msg -> completionMsg -> Model -> tagger -> Cmd msg
executeQuery query msg model tagger =
    let
        x =
            2
    in
        Cmd.none



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
