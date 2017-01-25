module Slate.Engine.Query exposing (NodeQuery, Query(..), query, buildQueryTemplate, parametricReplace, buildMessageDict, MessageDict, MessageDictEntry, AppEventMsg)

import String exposing (..)
import Dict exposing (..)
import Set exposing (..)
import List.Extra as ListE exposing (..)
import Regex exposing (HowMany(All, AtMost))
import Utils.Regex as RegexU
import Maybe.Extra as MaybeE exposing (isNothing)
import Utils.Ops exposing (..)
import Slate.Common.Schema exposing (..)
import Slate.Common.Event exposing (EventRecord)


type alias AppEventMsg msg =
    EventRecord -> msg


type alias NodeQuery msg =
    { properties : Maybe (List String)
    , criteria : Maybe String
    , schema : EntitySchema
    , msg : AppEventMsg msg
    }


type Query msg
    = Node (NodeQuery msg) (List (Query msg))
    | Leaf (NodeQuery msg)


emptySchema : EntitySchema
emptySchema =
    { type_ = ""
    , eventNames = []
    , properties = []
    }


query : AppEventMsg msg -> NodeQuery msg
query msg =
    { properties = Nothing
    , criteria = Nothing
    , schema = emptySchema
    , msg = msg
    }


depthDict : Query msg -> Dict Int (List (NodeQuery msg))
depthDict query =
    let
        add : NodeQuery msg -> Int -> Dict Int (List (NodeQuery msg)) -> Dict Int (List (NodeQuery msg))
        add nodeQuery depth dict =
            Dict.insert depth (nodeQuery :: Dict.get depth dict ?= []) dict

        addChildren : List (Query msg) -> Int -> Dict Int (List (NodeQuery msg)) -> Dict Int (List (NodeQuery msg))
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


toList : Query msg -> List (List (NodeQuery msg))
toList query =
    case query of
        Node nodeQuery children ->
            [ nodeQuery ] :: (List.map toFlatList children)

        Leaf nodeQuery ->
            [ [ nodeQuery ], [] ]


toFlatList : Query msg -> List (NodeQuery msg)
toFlatList query =
    case query of
        Node nodeQuery children ->
            nodeQuery :: (List.concat <| List.map toFlatList children)

        Leaf nodeQuery ->
            [ nodeQuery ]


toFlatListMap : (NodeQuery msg -> a) -> Query msg -> List a
toFlatListMap f =
    List.map f << toFlatList


toFlatList2 : Query msg -> List ( NodeQuery msg, List (Query msg) )
toFlatList2 query =
    case query of
        Node nodeQuery children ->
            ( nodeQuery, children ) :: (List.concat <| List.map toFlatList2 children)

        Leaf nodeQuery ->
            [ ( nodeQuery, [] ) ]


toFlatListMap2 : (NodeQuery msg -> List (Query msg) -> a) -> Query msg -> List a
toFlatListMap2 f2 =
    List.map (tupToArgs f2) << toFlatList2


tupToArgs : (a -> b -> c) -> ( a, b ) -> c
tupToArgs f ( a1, a2 ) =
    f a1 a2


propertiesCheck : NodeQuery msg -> List String
propertiesCheck nodeQuery =
    case nodeQuery.schema /= emptySchema of
        True ->
            let
                queryProperties =
                    Set.fromList <| nodeQuery.properties ?= []

                schemaProperties =
                    Set.fromList <| List.map .name nodeQuery.schema.properties

                diff =
                    Set.toList <| Set.diff queryProperties schemaProperties
            in
                List.map (flip (++) <| " is not a valid property for " ++ nodeQuery.schema.type_) diff

        False ->
            [ "Missing Schema in query node: " ++ (toString nodeQuery) ]


validQuery : Query msg -> List String
validQuery query =
    let
        propertiesErrors =
            List.concat <| toFlatListMap propertiesCheck query

        entityNames =
            toFlatListMap (.schema >> .type_) query
    in
        ((Set.size <| Set.fromList entityNames) /= List.length entityNames) ? ( [ "Query must NOT be cyclic" ], propertiesErrors )


extractQueryEntityName : Query msg -> String
extractQueryEntityName query =
    case query of
        Node nodeQuery children ->
            nodeQuery.schema.type_

        Leaf nodeQuery ->
            nodeQuery.schema.type_


extractNodeQuery : Query msg -> NodeQuery msg
extractNodeQuery query =
    case query of
        Node nodeQuery children ->
            nodeQuery

        Leaf nodeQuery ->
            nodeQuery


childEntityNames : NodeQuery msg -> List (Query msg) -> ( String, List (NodeQuery msg) )
childEntityNames nodeQuery children =
    ( nodeQuery.schema.type_, List.map extractNodeQuery children )


parentChild : Query msg -> Dict String (List (NodeQuery msg))
parentChild query =
    let
        filter =
            List.filter (\( _, list ) -> list /= [])

        parentChild =
            case (toFlatListMap2 childEntityNames query) of
                root :: [] ->
                    [ root ]

                root :: rest ->
                    root :: (filter rest)

                [] ->
                    []
    in
        Dict.fromList
            parentChild


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
SELECT id, ts, (extract(epoch from ts)*100000)::numeric AS trans_id, event{{maxIdColumn}}
FROM events
{{maxIdSQLClause}}
WHERE ({entityTemplates})
    AND {{additionalCriteria}}
    {{firstTemplateWithDataMaxCriteria}}
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
    ({entityIds}
        AND event#>>'{name}' IN ({eventNames})
        AND {entityCriteria}
        AND id > {{lastMaxId}})
"""


parametricReplace : String -> String -> List ( String, String ) -> String -> String
parametricReplace prefix suffix replacements template =
    let
        -- l =
        --     Debug.crash <| toString <| ( prefix, suffix, replacements, template )
        buildRegex param =
            Regex.escape <| prefix ++ param ++ suffix
    in
        List.foldl (\( param, value ) template -> RegexU.replaceAll (buildRegex param) value template) template replacements



-- List.foldl (\( param, value ) template -> RegexU.replace All (buildRegex param) (RegexU.simpleReplacer value) template) template replacements


propertySchemaEventNames : NodeQuery msg -> List ( EntitySchema, List String )
propertySchemaEventNames nodeQuery =
    let
        unwrap ( SchemaReference schema, eventNames ) =
            ( schema, eventNames )
    in
        List.map (\p -> ( p.entitySchema, p.eventNames )) nodeQuery.schema.properties
            |> List.filter (fst >> isNothing >> not)
            |> List.map (\( mes, ens ) -> ( mes |?> identity ?= SchemaReference mtEntitySchema, ens ))
            |> List.map unwrap


getEventNames : NodeQuery msg -> List (NodeQuery msg) -> List String
getEventNames parent children =
    let
        propertyEventNames : List String
        propertyEventNames =
            List.filter (\property -> List.member property.name (parent.properties ?= [])) parent.schema.properties
                |> List.map .eventNames
                |> List.concat

        parentPropertySchemaEventNames =
            propertySchemaEventNames parent

        findEventNames schema =
            snd <| (ListE.find (\( s, _ ) -> s == schema) parentPropertySchemaEventNames) ?= ( schema, [] )

        childrenEventNames : List String
        childrenEventNames =
            List.concat <| List.map (findEventNames << .schema) children
    in
        List.concat [ parent.schema.eventNames, propertyEventNames, childrenEventNames ]


templateReplace : List ( String, String ) -> String -> String
templateReplace =
    parametricReplace "{" "}"


type alias MessageDictEntry msg =
    { msg : AppEventMsg msg
    , maybeEntityType : Maybe String
    }


type alias MessageDict msg =
    Dict String (MessageDictEntry msg)


buildMessageDict : Query msg -> MessageDict msg
buildMessageDict query =
    let
        propertyEntityType : EntitySchemaReference -> String
        propertyEntityType schemaRef =
            case schemaRef of
                SchemaReference schema ->
                    schema.type_

        eventNames : EntitySchema -> List ( Maybe String, String )
        eventNames schema =
            schema.properties
                |> List.map (\property -> List.map ((,) <| property.entitySchema |?> propertyEntityType) property.eventNames)
                |> List.concat
                |> List.append (List.map ((,) Nothing) schema.eventNames)

        addToDict nodeQuery dict =
            List.foldl (\( maybeEntityType, name ) dict -> Dict.insert name (MessageDictEntry nodeQuery.msg maybeEntityType) dict) dict <| eventNames nodeQuery.schema

        build query dict =
            case query of
                Node nodeQuery children ->
                    List.foldl (\child dict -> build child dict) (addToDict nodeQuery dict) children

                Leaf nodeQuery ->
                    addToDict nodeQuery dict
    in
        build query Dict.empty


buildEntityTemplate : Dict String (List (NodeQuery msg)) -> NodeQuery msg -> String
buildEntityTemplate parentChild parent =
    let
        children =
            (Dict.get parent.schema.type_ parentChild ?= [])

        names =
            getEventNames parent children

        eventNames =
            String.join "," <| List.map (\name -> "'" ++ name ++ "'") names

        entityCriteria =
            parent.criteria ?= "1=1"
    in
        templateReplace
            [ ( "eventNames", eventNames )
            , ( "entityCriteria", entityCriteria )
            , ( "entityIds", "{{" ++ parent.schema.type_ ++ "-entityIds}}" )
            ]
            entityTemplate


buildSqlTemplate : Dict Int (List (NodeQuery msg)) -> Dict String (List (NodeQuery msg)) -> List String
buildSqlTemplate depthDict parentChild =
    let
        maxDepth =
            List.length <| Dict.keys depthDict

        build : Int -> List String -> List String
        build depth templates =
            let
                queriesAtDepth =
                    Dict.get depth depthDict ?= []

                entityTemplates =
                    String.join "\n\tOR " <| List.map (buildEntityTemplate parentChild) queriesAtDepth

                template =
                    templateReplace
                        [ ( "entityTemplates", entityTemplates )
                        ]
                        sqlTemplate

                newTemplates =
                    template :: templates
            in
                (depth > 0) ?! ( \_ -> build (depth - 1) newTemplates, \_ -> newTemplates )
    in
        build (maxDepth - 1) []


buildQueryTemplate : Query msg -> Result (List String) (List String)
buildQueryTemplate query =
    let
        errors =
            validQuery query
    in
        case errors == [] of
            True ->
                let
                    parentChildRelationships =
                        parentChild query
                in
                    Ok <| buildSqlTemplate (depthDict query) parentChildRelationships

            False ->
                Err errors
