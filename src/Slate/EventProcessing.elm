module Slate.EventProcessing exposing (..)

import String exposing (..)
import Date exposing (..)
import Maybe.Extra as MaybeE exposing (isNothing)
import Slate.Reference exposing (..)
import Slate.Event exposing (..)
import Utils.Ops exposing (..)


-- Getters from Event Data


getConvertedValue : (String -> Result String value) -> Event -> Result String value
getConvertedValue convert event =
    let
        result =
            checkValueExists event <| event.data.value
    in
        case result of
            Ok v ->
                convert v

            Err msg ->
                Err msg


getIntValue : Event -> Result String Int
getIntValue event =
    getConvertedValue String.toInt event


getFloatValue : Event -> Result String Float
getFloatValue event =
    getConvertedValue String.toFloat event


getDateValue : Event -> Result String Date
getDateValue event =
    getConvertedValue Date.fromString event


checkExists : String -> Event -> Maybe value -> Result String value
checkExists type_ event value =
    case value of
        Just v ->
            Ok v

        Nothing ->
            Err <| "Event data " ++ type_ ++ " is missing " ++ (toString event)


checkValueExists : Event -> Maybe value -> Result String value
checkValueExists =
    checkExists "value"


checkReferenceExists : Event -> Maybe value -> Result String value
checkReferenceExists =
    checkExists "reference"


getStringValue : Event -> Result String String
getStringValue event =
    checkValueExists event <| event.data.value


getReference : Event -> Result String EntityReference
getReference event =
    checkReferenceExists event <| event.data.referenceId


{-| Update entity property value
-}
updatePropertyValue : (Event -> Result String value) -> (Maybe value -> entity -> entity) -> Event -> entity -> Result String entity
updatePropertyValue get update event entity =
    let
        value =
            get event
    in
        case value of
            Ok val ->
                Ok (update (Just val) entity)

            Err msg ->
                Err msg


{-| Update entity property reference
-}
updatePropertyReference : (Maybe EntityReference -> entity -> entity) -> Event -> entity -> Result String entity
updatePropertyReference =
    updatePropertyValue getReference


{-| Update entity property list by appending (positioning is done by another event)
-}
updatePropertyList : (Event -> Result String listValue) -> (listValue -> entity -> entity) -> Event -> entity -> Result String entity
updatePropertyList get update event entity =
    let
        listValue =
            get event
    in
        case listValue of
            Ok listVal ->
                Ok (update listVal entity)

            Err msg ->
                Err msg


{-| Position entity property list
-}
positionPropertyList : Maybe (List value) -> (Maybe (List value) -> entity -> entity) -> Event -> entity -> Result String entity
positionPropertyList maybeList update event entity =
    let
        list =
            maybeList ?= []

        invalidMove =
            newPosition >= length - 1 || oldPosition >= length

        errors =
            (List.map (((++) " is missing") << snd) <| List.filter (isNothing << fst) [ ( event.data.oldPosition, "Old Position" ), ( event.data.newPosition, "New Position" ) ])
                |> List.append (invalidMove ? ( [ "Positions are out of bounds" ++ (toString event) ], [] ))

        ( oldPosition, newPosition ) =
            ( event.data.oldPosition ?= 0, event.data.newPosition ?= 0 )

        length =
            List.length list
    in
        case errors == [] of
            True ->
                let
                    item =
                        List.take 1 (List.take oldPosition list)

                    removed =
                        List.append (List.take oldPosition list) (List.drop (oldPosition + 1) list)

                    inserted =
                        List.append (List.take oldPosition list) (List.append item <| List.drop oldPosition list)
                in
                    Ok <| update (Just inserted) entity

            False ->
                Err <| String.join "\n" errors



-- 1 2 [3] 4 5 6 7
-- old = 2
-- 1 2 4 5 6 7
-- new = 3
-- 1 2 4 [3] 5 6 7


{-| Append to a property list
-}
appendPropertyList : Maybe (List listValue) -> listValue -> Maybe (List listValue)
appendPropertyList list value =
    Just <| List.append (list ?= []) [ value ]


{-| Append to a property list
-}
setPropertyList : Maybe (List listValue) -> listValue -> Maybe (List listValue)
setPropertyList list value =
    Just [ value ]