module Json.Helper exposing (..)

import Dict exposing (..)
import Json.Decode as JD exposing (..)
import Json.Encode as JE exposing (..)
import Utils.Utils exposing (..)


(//) : Maybe a -> a -> a
(//) =
    flip Maybe.withDefault


(////) : JD.Decoder a -> a -> JD.Decoder a
(////) decoder default =
    (maybe decoder) `JD.andThen` (\maybe -> JD.succeed (maybe // default))


(<||) : JD.Decoder (a -> b) -> JD.Decoder a -> JD.Decoder b
(<||) =
    JD.object2 (<|)


encMaybe : (a -> JE.Value) -> Maybe a -> JE.Value
encMaybe encoder maybe =
    (Maybe.map (\just -> encoder just) maybe) // JE.null


encDict : (comparable -> JE.Value) -> (value -> JE.Value) -> Dict comparable value -> JE.Value
encDict keyEncoder valueEncoder dict =
    JE.object
        [ ( "keys", JE.list <| List.map keyEncoder <| Dict.keys dict )
        , ( "values", JE.list <| List.map valueEncoder <| Dict.values dict )
        ]


decConvertDict : (a -> value) -> Decoder comparable -> Decoder a -> Decoder (Dict comparable value)
decConvertDict valuesConverter keyDecoder valueDecoder =
    let
        makeDict keys values =
            Dict.fromList <| sndMap valuesConverter <| List.map2 (,) keys values
    in
        JD.object2 makeDict ("keys" := JD.list keyDecoder) ("values" := JD.list valueDecoder)


decDict : Decoder comparable -> Decoder value -> Decoder (Dict comparable value)
decDict =
    decConvertDict identity



-- decDict : Decoder comparable -> Decoder value -> Decoder (Dict comparable value)
-- decDict keyDecoder valueDecoder =
--     let
--         makeDict keys values =
--             Dict.fromList <| List.map2 (,) keys values
--     in
--         JD.object2 makeDict ("keys" := JD.list keyDecoder) ("values" := JD.list valueDecoder)