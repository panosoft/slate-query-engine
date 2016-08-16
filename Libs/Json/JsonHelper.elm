module Json.JsonHelper exposing (..)

import Json.Decode as Json exposing ((:=), maybe, string, int, float)


(//) : Maybe a -> a -> a
(//) =
    flip Maybe.withDefault


(///) : Json.Decoder a -> a -> Json.Decoder a
(///) decoder default =
    (maybe decoder) `Json.andThen` (\maybe -> Json.succeed (maybe // default))


(<||) : Json.Decoder (a -> b) -> Json.Decoder a -> Json.Decoder b
(<||) =
    Json.object2 (<|)
