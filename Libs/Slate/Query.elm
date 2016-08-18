module Slate.Query exposing (NodeQuery, Query(..), query, buildQueryTemplate, parametricReplace)

import String exposing (..)
import Dict exposing (..)
import Set exposing (..)
import Slate.Schema exposing (..)
import Utils.Utils exposing (filterJust)
import Regex exposing (HowMany(All, AtMost))
import Regex.Extra as RE exposing (..)


type alias NodeQuery =
    { properties : Maybe (List String)
    , criteria : Maybe String
    , schema : Maybe EntitySchema
    }


type Query
    = Node NodeQuery (List Query)
    | Leaf NodeQuery


query : NodeQuery
query =
    { properties = Nothing
    , criteria = Nothing
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


entityName : NodeQuery -> String
entityName nodeQuery =
    case nodeQuery.schema of
        Just schema ->
            schema.type'

        Nothing ->
            ""


toFlatList : Query -> List NodeQuery
toFlatList query =
    case query of
        Node nodeQuery children ->
            nodeQuery :: (List.concat <| List.map toFlatList children)

        Leaf nodeQuery ->
            [ nodeQuery ]


toFlatListMap : (NodeQuery -> a) -> Query -> List a
toFlatListMap f =
    List.map f << toFlatList


toFlatList2 : Query -> List ( NodeQuery, List Query )
toFlatList2 query =
    case query of
        Node nodeQuery children ->
            ( nodeQuery, children ) :: (List.concat <| List.map toFlatList2 children)

        Leaf nodeQuery ->
            [ ( nodeQuery, [] ) ]


toFlatListMap2 : (NodeQuery -> List Query -> a) -> Query -> List a
toFlatListMap2 f2 =
    List.map (tupToArgs f2) << toFlatList2


tupToArgs : (a -> b -> c) -> ( a, b ) -> c
tupToArgs f ( a1, a2 ) =
    f a1 a2



-- map : (NodeQuery -> a) -> Query -> List a
-- map f query =
--     case query of
--         Node nodeQuery children ->
--             f nodeQuery :: (List.concat <| List.map (map f) children)
--
--         Leaf nodeQuery ->
--             [ f nodeQuery ]
--
--
--
-- map2 : (NodeQuery -> List Query -> a) -> Query -> List a
-- map2 f2 query =
--     case query of
--         Node nodeQuery children ->
--             f2 nodeQuery children :: (List.concat <| List.map (map2 f2) children)
--
--         Leaf nodeQuery ->
--             [ f2 nodeQuery [] ]


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
            List.concat <| toFlatListMap propertiesCheck query

        entityNames =
            toFlatListMap extractEntityName query
    in
        if (Set.size <| Set.fromList entityNames) /= List.length entityNames then
            [ "Query must NOT be cyclic" ]
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
    Dict.fromList <| List.filter (\( _, list ) -> list /= []) <| toFlatListMap2 childEntityNames query



-- TODO finish


{-| clause to be added on first query of set
-}
maxIdSqlClause : String
maxIdSqlClause =
    "CROSS JOIN (SELECT MAX(id) FROM events) AS q"


{-| maxColumn and maxIdSQLClause are to be added on first query only. This will precludes subsequent
    queries from including information that was not available when the first query is executed.

    additionalCriteria is the optional criteria for the whole query.

    firstQueryMaxCriteria is of the form `AND id < {{firstQueryMaxId}}`, where firstQueryMaxId is from the first query in the template.
    For the first query this should be BLANK.

    N.B. Parameters with { are replaced in this module
         Parameters with {{}} are replaced in the Query Engine
-}
sqlTemplate : String
sqlTemplate =
    """
SELECT id, ts, (extract(epoch from ts)*100000)::numeric AS trans_id, event{maxIdColumn}
FROM events
{maxIdSQLClause}
WHERE ({entityTemplates})
    AND {{additionalCriteria}}
    {{firstQueryMaxCriteria}}
ORDER BY id
"""


{-| entityIds are the entity ids for the query. In the first query they are provided as part of the buildQuery call.
    In subsequent queries, they are the ids of the children that were retrieved by the parent.

    eventNames are the events of the current node and direct children.

    entityCriteria is an optional criteria for this node.

    lastMaxId is the max Id of the current dataset. Initially, this is -1.

    N.B. Parameters with { are replaced in this module
         Parameters with {{}} are replaced in the Query Engine
-}
entityTemplate : String
entityTemplate =
    """
(entity_id IN ({{entityIds}})
        AND event->>'name' IN ({eventNames})
        AND {entityCriteria}
        AND id > {{lastMaxId}})
"""


parametricReplace : String -> String -> String -> List ( String, String ) -> String
parametricReplace prefix suffix template replacements =
    let
        buildRegex param =
            Regex.escape <| prefix ++ param ++ suffix
    in
        List.foldl (\( param, value ) template -> RE.replace All (buildRegex param) (RE.simpleReplacer value) template) template replacements


getEventNames : Dict String (List NodeQuery) -> NodeQuery -> List String
getEventNames parentChild parent =
    let
        children =
            (Dict.get (extractEntityName parent) parentChild) // []

        nodeQueries =
            parent :: children

        schemas =
            Utils.Utils.filterJust <| List.map .schema nodeQueries
    in
        List.concat <| List.map .eventNames schemas


templateReplace : String -> List ( String, String ) -> String
templateReplace =
    parametricReplace "{" "}"


buildEntityTemplate : Dict String (List NodeQuery) -> NodeQuery -> String
buildEntityTemplate parentChild nodeQuery =
    let
        names =
            getEventNames parentChild nodeQuery

        eventNames =
            String.join "," <| List.map (\name -> "'name'") names

        entityCriteria =
            nodeQuery.criteria // ""
    in
        templateReplace entityTemplate
            [ ( "eventNames", eventNames )
            , ( "entityCriteria", entityCriteria )
            ]


buildSqlTemplate : Dict Int (List NodeQuery) -> Dict String (List NodeQuery) -> List String
buildSqlTemplate queriesAtDepth parentChild =
    let
        maxDepth =
            List.length <| Dict.keys queriesAtDepth

        build : Int -> List String -> List String
        build depth templates =
            let
                queries =
                    Dict.get depth queriesAtDepth // []

                maxIdColumn =
                    if depth == 0 then
                        ", q.max"
                    else
                        ""

                maxIdSQLClause =
                    if depth == 0 then
                        "CROSS JOIN (SELECT MAX(id) FROM events) AS q"
                    else
                        ""

                entityTemplates =
                    String.join "\n\tOR " <| List.map (buildEntityTemplate parentChild) queries

                template =
                    templateReplace entityTemplate
                        [ ( "maxIdColumn", maxIdColumn )
                        , ( "maxIdSQLClause", maxIdSQLClause )
                        , ( "entityTemplates", entityTemplates )
                        ]

                newTemplates =
                    template :: templates
            in
                if depth < maxDepth - 1 then
                    build (depth + 1) newTemplates
                else
                    newTemplates
    in
        List.reverse <| build 0 []


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

                parentChildRelationships =
                    parentChild query
            in
                Ok []
