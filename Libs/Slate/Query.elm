module Slate.Query exposing (NodeQuery, Query(..), query, buildQueryTemplate)

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


map : (NodeQuery -> a) -> Query -> List a
map f query =
    case query of
        Node nodeQuery children ->
            f nodeQuery :: (List.concat <| List.map (map f) children)

        Leaf nodeQuery ->
            [ f nodeQuery ]


map2 : (NodeQuery -> List Query -> a) -> Query -> List a
map2 f2 query =
    case query of
        Node nodeQuery children ->
            f2 nodeQuery children :: (List.concat <| List.map (map2 f2) children)

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
            List.concat <| map propertiesCheck query

        entityNames =
            map extractEntityName query
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
    Dict.fromList <| List.filter (\( _, list ) -> list /= []) <| map2 childEntityNames query



-- TODO finish


{-| clause to be added on first query of set
-}
maxIdSQLClause : String
maxIdSQLClause =
    "CROSS JOIN (SELECT MAX(id) FROM events) AS q"


{-| column to be added on first query of set
-}
maxIdColumn : String
maxIdColumn =
    ", q.max"


{-| maxColumn and maxIdSQLClause are to be added on first query only. This will precludes subsequent
    queries from including information that was not available when the first query is executed.

    entityIds are the entity ids for the query. In the first query they are provided as part of the buildQuery call.
    In subsequent queries, they are the ids of the children that were retrieved by the parent.

    eventNames are the events of the current node and direct children.

    entityCriteria is an optional criteria for this node.

    lastMaxId is the max Id of the current dataset. Initially, this is -1.

    additionalCriteria is the optional criteria for the whole query.

    firstQueryMaxCriteria is of the form `AND id < {{firstQueryMaxId}}`, where firstQueryMaxId is from the first query in the template.
    For the first query this should be BLANK.
-}
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
    {{firstQueryMaxCriteria}}
ORDER BY id
"""


buildQueryTemplate : Query -> Result (List String) (List String)
buildQueryTemplate query =
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
