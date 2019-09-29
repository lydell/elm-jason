module Jason.Value exposing (JsonValue(..), fromCoreValue, fromString, toCoreValue, toString)

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode
import Json.Encode


type JsonValue
    = JsonString String
    | JsonNumber Float
    | JsonBool Bool
    | JsonNull
    | JsonArray (Array JsonValue)
    | JsonObject (Dict String JsonValue)
    | Missing


decoder : Json.Decode.Decoder JsonValue
decoder =
    Json.Decode.oneOf
        [ Json.Decode.map JsonString Json.Decode.string
        , Json.Decode.map JsonNumber Json.Decode.float
        , Json.Decode.map JsonBool Json.Decode.bool
        , Json.Decode.null JsonNull
        , Json.Decode.map JsonArray (Json.Decode.array decoderLazy)
        , Json.Decode.map JsonObject (Json.Decode.dict decoderLazy)
        ]


decoderLazy : Json.Decode.Decoder JsonValue
decoderLazy =
    Json.Decode.lazy (\() -> decoder)


fromCoreValue : Json.Decode.Value -> JsonValue
fromCoreValue coreValue =
    case Json.Decode.decodeValue decoder coreValue of
        Ok jsonValue ->
            jsonValue

        Err _ ->
            Missing


toCoreValue : JsonValue -> Json.Decode.Value
toCoreValue jsonValue =
    case jsonValue of
        JsonString str ->
            Json.Encode.string str

        JsonNumber num ->
            Json.Encode.float num

        JsonBool boolean ->
            Json.Encode.bool boolean

        JsonNull ->
            Json.Encode.null

        JsonArray arr ->
            Json.Encode.array toCoreValue (Array.filter ((/=) Missing) arr)

        JsonObject dictionary ->
            Json.Encode.dict identity toCoreValue (Dict.filter (always ((/=) Missing)) dictionary)

        Missing ->
            Json.Encode.null


fromString : String -> Result String JsonValue
fromString str =
    case Json.Decode.decodeString decoder str of
        Ok val ->
            Ok val

        Err err ->
            Err (Json.Decode.errorToString err)


toString : Int -> JsonValue -> String
toString indentLevel jsonValue =
    Json.Encode.encode indentLevel (toCoreValue jsonValue)
