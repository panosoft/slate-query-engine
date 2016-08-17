module Slate.Query exposing (NodeQuery, Query(..), query, buildQuery)

import Dict exposing (..)
import Set exposing (..)
import Slate.Schema exposing (..)


type alias NodeQuery =
    { properties : Maybe (List String)
    , additionalCriteria : Maybe String
    , schema : Maybe EntitySchema
    }


type Query
    = Node NodeQuery (List Query)
    | Leaf NodeQuery


query : NodeQuery
query =
    { properties = Nothing
    , additionalCriteria = Nothing
    , schema = Nothing
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


entityName nodeQuery =
    case nodeQuery.schema of
        Just schema ->
            schema.type'

        Nothing ->
            ""


fold : (NodeQuery -> a) -> Query -> List a
fold f query =
    case query of
        Node nodeQuery children ->
            f nodeQuery :: (List.concat <| List.map (fold f) children)

        Leaf nodeQuery ->
            [ f nodeQuery ]


fold2 : (NodeQuery -> List Query -> a) -> Query -> List a
fold2 f2 query =
    case query of
        Node nodeQuery children ->
            f2 nodeQuery children :: (List.concat <| List.map (fold2 f2) children)

        Leaf nodeQuery ->
            [ f2 nodeQuery [] ]


propertiesCheck : NodeQuery -> List String
propertiesCheck nodeQuery =
    case nodeQuery.schema of
        Just schema ->
            let
                queryProperties =
                    Set.fromList <| nodeQuery.properties // []

                schemaProperties =
                    Set.fromList <| List.map .type' schema.properties

                diff =
                    Set.toList <| Set.diff queryProperties schemaProperties
            in
                List.map (flip (++) <| " is not a valid property for " ++ schema.type') diff

        Nothing ->
            [ "Missing Schema in query node: " ++ (toString nodeQuery) ]


extractEntityName : NodeQuery -> String
extractEntityName nodeQuery =
    case nodeQuery.schema of
        Just schema ->
            schema.type'

        Nothing ->
            Debug.crash "Cannot extract entity name - Missing Schema - PROGRAM BUG"


validQuery : Query -> List String
validQuery query =
    let
        propertiesErrors =
            List.concat <| fold propertiesCheck query

        entityNames =
            fold extractEntityName query
    in
        if (Set.size <| Set.fromList entityNames) /= List.length entityNames then
            [ "Query must NOT be Cyclic" ]
        else
            propertiesErrors


extractQueryEntityName : Query -> String
extractQueryEntityName query =
    case query of
        Node nodeQuery children ->
            extractEntityName nodeQuery

        Leaf nodeQuery ->
            extractEntityName nodeQuery


childEntityNames : NodeQuery -> List Query -> ( String, List String )
childEntityNames nodeQuery children =
    ( extractEntityName nodeQuery, List.map extractQueryEntityName children )


parentChild : Query -> Dict String (List String)
parentChild query =
    Dict.fromList <| List.filter (\( _, list ) -> list /= []) <| fold2 childEntityNames query



-- TODO finish


maxIdSQLClause : String
maxIdSQLClause =
    "CROSS JOIN (SELECT MAX(id) FROM events) AS q"


maxIdColumn : String
maxIdColumn =
    ", q.max"


sQLTemplate : String
sQLTemplate =
    """
SELECT id, ts, (extract(epoch from ts)*100000)::numeric AS trans_id, event{{maxIdColumn}}
FROM events
{{maxIdSQLClause}}
WHERE ((entity_id IN ({{entityIds}})
        AND event->>'name' IN ({{eventNames}})
        AND {{entityCriteria}}
        AND id > {{lastMaxId}}))
    AND {{additionalCriteria}}
    AND {{firstQueryMaxId}}
ORDER BY id
"""


buildQuery : String -> Query -> Result (List String) (List String)
buildQuery additionalCriteria query =
    let
        errors =
            validQuery query
    in
        if errors /= [] then
            Err errors
        else
            let
                queriesAtDepth =
                    depthDict query
            in
                Ok []
