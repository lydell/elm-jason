module Jason.Value exposing (JsonValue(..), fromCoreValue, fromString, toCoreValue, toString)

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode
import Json.Encode


{-| An Elm representation of JSON values.

`Missing` isn’t really a JSON value, but can be nice if you ever need to deal
with JSON where it matters if a field is missing altogether or set to `null`.
When turning a `JsonValue` to a string, fields and array items set to `Missing`
are omitted.

    toString 0 (JsonArray (Array.fromList [ JsonNumber 1, Missing, JsonNumber 2 ])) == "[1,2]"

Rather than having a `Jason.Encode` module, use the value constructors of
`JsonValue` directly, and then pass that to `toString`. It’s recommended to
import like this:

    import Jason.Value exposing (JsonValue(..))

Since all the value names (except `Missing`) start with `Json` it’s clear
where they come from.

-}
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


{-| Turn a `Json.Decode.Value` into our `JsonValue`.
-}
fromCoreValue : Json.Decode.Value -> JsonValue
fromCoreValue coreValue =
    case Json.Decode.decodeValue decoder coreValue of
        Ok jsonValue ->
            jsonValue

        Err _ ->
            Missing


{-| Turn our `JsonValue` into a `Json.Decode.Value`.
-}
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

        JsonArray array ->
            Json.Encode.array toCoreValue (Array.filter ((/=) Missing) array)

        JsonObject dictionary ->
            Json.Encode.dict identity toCoreValue (Dict.filter (always ((/=) Missing)) dictionary)

        Missing ->
            Json.Encode.null


{-| Parse a JSON string into a `JsonValue`. If the JSON string has invalid
syntax, a string error message is returned.

You can think of this like `JSON.parse` in JavaScript.

The `JsonValue` returned by this function isn’t very useful in itself. Use
the decoders in Jason.Decode to turn it into a nicer data structure.

-}
fromString : String -> Result String JsonValue
fromString str =
    case Json.Decode.decodeString decoder str of
        Ok value ->
            Ok value

        Err error ->
            Err (Json.Decode.errorToString error)


{-| Turn a `JsonValue` into a string.

You can think of this like `JSON.stringify` in JavaScript.

If you want to turn an Elm value into a JSON string, it is up to you to first
contruct a `JsonValue` from it by using the value constructors of
`JsonValue`.

-}
toString : Int -> JsonValue -> String
toString indentLevel jsonValue =
    Json.Encode.encode indentLevel (toCoreValue jsonValue)
