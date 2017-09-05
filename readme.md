# DEPRECATED - Please see [elm-slate/query](https://github.com/elm-slate/query)


# Slate Query Engine

> Query Engine for Slate that allows Queries to be defined in Elm. The Engine takes care of all of the complexity of building the SQL queries. This is the heart of Slate since most of the code written to use Slate will use the Engine for queries.

## Install

### Elm

Since the Elm Package Manager doesn't allow for Native code and most everything we write at Panoramic Software has some native code in it,
you have to install this library directly from GitHub, e.g. via [elm-github-install](https://github.com/gdotdesign/elm-github-install) or some equivalent mechanism. It's just not worth the hassle of putting libraries into the Elm package manager until it allows native code.

## Entities

`Entities` in Slate are records or objects that have `Properties` (see below). Entities can have relationships with other `Entites`. Those relationships can optionally be an ownership.

## Properties

`Properties` can be primitive types that can be represented as a `String` or a `Value Object` which is represented as a JSON string. `Value Objects` are atomically mutated, i.e. you cannot just change a part of the `Value Object`, e.g. imagine the following:

```elm
type alias EntirePerson =
    { name : Maybe Name
    , age : Maybe Int
    , address : Maybe EntityReference
    }

type alias Name =
    { first : String
    , middle : String
    , last : String
    }
```

Here `Name` is a property of `EntirePerson`. When changing the `Name`, the whole record will be replaced. There is NO support for Events that only modify the `middle` of `Name`. The whole `Name` will be replaced making it an atomic mutation.

If there were a case where the individual fields of `Name` needed to be mutated independently, then this `Value Object` should be implemented as an `Entity`.

## Queries

`Queries` in Slate are defined as a tree. There is 1 root node `Entity Type`. Each child node is another `Entity Type` that has a relationships with its parent node. So for example, imagine 2 `Entities`, a `Person` and an `Address`:

```elm
type alias EntirePerson =
    { name : Maybe Name
    , age : Maybe Int
    , address : Maybe EntityReference
    }

type alias EntireAddress =
    { street : Maybe String
    , city : Maybe String
    , state : Maybe String
    , zip : Maybe String
    }
```

In this example, `Person` has a relationship with `Address`. (Actually, `Person` `owns` `Address`, but that's defined in the `Person Schema` which is not shown.)

So one could query the Slate DB in 2 different ways.

1. Get all (or a select group of) `Addresses` and their corresponding `Persons`
2. Get all (or a select group of) `Persons` and their corresponding `Addresses`

Depending on the structure of the Entity Design, one query may be more efficient than the other. For example, if `Addresses` are owned by many different entities, then Query #1 would be inefficient since it would bring back far more `Addresses` than `Persons`.

Here is an example of how Query #2 could be defined in Elm as:

```elm
query : NodeQuery Msg
query =
    Slate.Engine.Query.mtQuery UnspecifiedMutationInQuery


personQuery : Query Msg
personQuery =
    Node { query | schema = personSchema, properties = Just [ "name" ], tagger = MutatePerson }
        [ Leaf { query | schema = addressSchema, properties = Just [ "street" ], tagger = MutateAddress } ]
```

Here a `Node` contains 1 or more children and `Leaf` is a terminal node in the `Query Tree`. Each node contains a `Query`.

Parent nodes have relationships with child nodes.

`personQuery` first queries for `Persons` then the associated `Addresses`.

The `properties` field can be used to define which properties are returned. If omitted then all properties are returned. To return NO properties simple provide an empty list, i.e. [].

See `Test.App` for how to implement `Queries`.

Additional information can be found in [slate-common](https://github.com/panosoft/slate-common).

## API

### Engine

#### Config

Slate Engine configuration.

```elm
type alias Config msg =
    { logTagger : ( LogLevel, ( QueryStateId, String ) ) -> msg
    , errorTagger : ( ErrorType, ( QueryStateId, String ) ) -> msg
	, eventProcessingErrorTagger : ( String, String ) -> msg
    , completionTagger : Int -> msg
    , routeToMeTagger : Msg -> msg
    , queryBatchSize : Int
    }
```

* `logTagger` - Msg constructor for logging for the Engine's parent.
* `errorTagger` - Msg constructor for errors for the Engine's parent.
* `eventProcessingErrorTagger` - Msg constructor for event processing errors for the client of the Engine.
* `completionTagger` - Msg constructor for query completion for the client of the Engine.
* `routeToMeTagger` - Msg constructor to route a msg back to the Engine.
* `queryBatchSize` - Number of events to return per retreival from the Slate Database.

#### Model msg

The Engine's model has a Type Parameter `msg` which is the Event Processor's `Msg`.

#### Msg

The Engine's Msg type.

#### initModel

Initial model for the Engine.

```elm
initModel : Model msg
initModel
```

#### update

Engine's update function.

```elm
update : Config msg -> Msg -> Model msg -> ( ( Model msg, Cmd Msg ), List msg )
update config msg model
```

__Usage__

The following code snippet uses [elm-parent-child-update](https://github.com/panosoft/elm-parent-child-update):

```elm
type alias Model =
    { engineModel : Engine.Model Msg
    , ...
    }

type Msg
    = SlateEngine Engine.Msg
	  ...

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateEngine : Engine.Msg -> Model -> ( Model, Cmd Msg )
        updateEngine =
            ParentChildUpdate.updateChildApp (Engine.update engineConfig) update .engineModel SlateEngine (\model engineModel -> { model | engineModel = engineModel })
	in
		case msg of
			...		
```

See `Test/App.elm` in this repo for usage in context.

#### executeQuery

Execute Slate Query.

```elm
executeQuery : Config msg -> DBConnectionInfo -> Model msg -> Maybe String -> Query msg -> List String -> Result (List String) ( Model msg, Cmd msg, Int )
executeQuery config dbInfo model additionalCriteria query rootIds
```

* `config` - The Engine's configuration.
* `dbInfo` - DB Connection info.
* `model` - The Engine's model.
* `additionalCriteria` - [OPTIONAL] This is a criteria that will be added to EVERY `Entity` in the `Query`. It should be written as a SQL `WHERE` clause minus the `WHERE`.
* `query` - The Slate `Query`.
* `rootIds` - The `Entity Ids` for the root `Entity`. If this `List` is empty then ALL Entities will be returned.

__Returns__

> Result:

> ERROR - errors

> SUCCESS - (model, cmd, queryStateId)

* `queryStateId` - This is the id of the `QueryState` that uniquely identifies this `Query` from other `Queries` that the `Engine` is maintaining.

__Usage__

See `Test/App.elm` in this repo for usage in context.

#### refreshQuery

Refresh an existing Slate Query, i.e. process events since the last `executeQuery` or `refreshQuery`.

```elm
refreshQuery : Config msg -> DbConnectionInfo -> Model msg -> QueryStateId -> Result String ( Model msg, Cmd msg )
refreshQuery config dbInfo model queryStateId
```

* `config` - The Engine's configuration.
* `dbInfo` - DB Connection info.
* `model` - The Engine's model.
* `queryStateId` - The QueryState id, i.e. the Query to refresh.

__Returns__

> Result:

> ERROR - "Invalid QueryStateId"

> SUCCESS - (model, cmd)

#### disposeQuery

Stop managing specified `Query`. Afterwards, the `queryStateId` will no longer be valid.

```elm
disposeQuery : Model msg -> Int -> Model msg
disposeQuery model queryStateId
```

* `model` - The Engine's model.
* `queryStateId` - The QueryState id, i.e. the Query to refresh.

#### exportQueryState

Create a JSON String for saving the specified `QueryState`.

This is useful for saving a query for a subsequent execution of your App.

```elm
exportQueryState : Model msg -> QueryStateId -> Result String String
exportQueryState model queryStateId
```

* `model` - The Engine's model.
* `queryStateId` - The QueryState id, i.e. the Query to refresh.

__Returns__

> Result:

> ERROR - "Invalid QueryStateId"

> SUCCESS - JSON

#### importQueryState

Recreate a previously saved `QueryState` from the specified JSON String.

```elm
importQueryState : Query msg -> Model msg -> String -> Result String (Model msg)
importQueryState query model json
```

* `model` - The Engine's model.
* `query` - The Slate `Query`.
* `json` - A JSON string created from `exportQueryState`


### Query

#### mtQuery

Convenience function to make defining queries easier.

This generates an empty query that can be mutated using the terse syntax in Elm.

```elm
type alias EventTagger msg =
    EventRecord -> msg

mtQuery : EventTagger msg -> NodeQuery msg
mtQuery unhandledSpecifiedTagger =
    { properties = Nothing
    , criteria = Nothing
    , schema = mtEntitySchema
    , tagger = unhandledSpecifiedTagger
    }
```

* `properties` - [OPTIONAL]This defines which properties are to be returned. If omitted, then all will be returned. To return NO properties, set this to [].
* `criteria` - [OPTIONAL] This criteria will be applied ONLY to quering this Entity. To apply a criteria to ALL Entities see [Engine.executeQuery](#executequery). It should be written as a SQL `WHERE` clause minus the `WHERE`.
* `schema` - This is the schema of the Entity that's part of the query.
* `tagger` - Msg constructor to create a Msg from an `EventRecord`. When this message is received by the initiator of the query, it will typically do mutation operations on the Entity specified by the `EventRecord`.
* `unhandledSpecifiedTagger` - Msg constructor that handles queries that fail to supply `tagger`. This should be considered a FATAL error.


#### buildQueryTemplate

Build Query Template from a query.

```elm
buildQueryTemplate : Query msg -> Result (List String) (List String)
buildQueryTemplate query
```

This function is used by the Engine and is not usually needed by clients of the Engine.

#### parametricReplace

Parametric replacement of a template where the `prefix` and `suffix` define the delimiters of the parameters.

```elm
parametricReplace : String -> String -> List ( String, String ) -> String -> String
parametricReplace prefix suffix replacements template =
```

This function is used by the Engine and is not usually needed by clients of the Engine.

#### buildMsgDict

Build a Msg Dictionary that relates the Slate Event Name to the taggers in the query.

```elm
buildMsgDict : Query msg -> MsgDict msg
buildMsgDict query =
```

This function is used by the Engine and is not usually needed by clients of the Engine.
