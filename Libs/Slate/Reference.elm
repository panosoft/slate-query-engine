module Slate.Reference exposing (..)

import Dict exposing (Dict)
import Json.Encode as JE exposing (..)
import Json.Decode as JD exposing (..)
import Slate.Event exposing (Event)


type alias EntityReference =
    String


(//) : Maybe a -> a -> a
(//) =
    flip Maybe.withDefault


lookupEntity : Dict String entity -> Event -> entity -> entity
lookupEntity entities event default =
    dereferenceEntity entities (Just event.data.entityId) default


dereferenceEntity : Dict String entity -> Maybe EntityReference -> entity -> entity
dereferenceEntity entities ref default =
    Dict.get (ref // "") entities // default


entityReferenceEncode : EntityReference -> JE.Value
entityReferenceEncode =
    JE.string


entityReferenceDecoder : Decoder EntityReference
entityReferenceDecoder =
    JD.string
