module Slate.Query exposing (..)

import Dict exposing (..)


type alias NodeQuery =
    { entity : String
    , properties : Maybe (List String)
    , additionalCriteria : Maybe String
    }


type Query
    = Node NodeQuery (List Query)
    | Leaf NodeQuery


query : NodeQuery
query =
    { entity = ""
    , properties = Nothing
    , additionalCriteria = Nothing
    }


(//) : Maybe a -> a -> a
(//) =
    flip Maybe.withDefault


depthDict : Query -> Dict Int (List NodeQuery)
depthDict query =
    let
        add : NodeQuery -> Int -> Dict Int (List NodeQuery) -> Dict Int (List NodeQuery)
        add nodeQuery depth dict =
            Dict.insert depth (nodeQuery :: Dict.get depth dict // []) dict

        addChildren : List Query -> Int -> Dict Int (List NodeQuery) -> Dict Int (List NodeQuery)
        addChildren children depth dict =
            case children of
                child :: rest ->
                    depthDict' child depth <| addChildren rest depth dict

                [] ->
                    dict

        depthDict' query depth dict =
            case query of
                Node nodeQuery children ->
                    add nodeQuery depth <| addChildren children (depth + 1) dict

                Leaf nodeQuery ->
                    add nodeQuery depth dict
    in
        depthDict' query 0 Dict.empty



-- TODO finish


buildQuery : String -> Query -> List String
buildQuery additionalCriteria query =
    let
        queriesAtDepth =
            Debug.log "depthDict" <| depthDict query
    in
        []



-- TODO remove example code


exQuery : List String
exQuery =
    Node { query | entity = "A", properties = Just [ "i", "status" ] }
        [ Leaf { query | entity = "B", properties = Just [] }
        , Node { query | entity = "C" }
            [ Leaf
                { query | entity = "E" }
            , Leaf
                { query | entity = "F", properties = Just [ "n", "o" ] }
            ]
        , Leaf { query | entity = "X" }
        ]
        |> buildQuery ""
