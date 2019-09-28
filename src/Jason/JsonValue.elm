module Jason.JsonValue exposing
    ( JsonValue(..)
    , Value(..)
    , decoder
    , encode
    , unwrap
    )

{-| This is the backbone data type for this entire package. The regular
`Json.Decode.decodeString` is used to turn raw JSON into this data type,
`JsonValue`. The rest of the package then operate on `JsonValue`, refining
it further into actually usable Elm data types. Finally, the regular
`Json.Encode.encode` function is used to turn it back to a string again.

This module exposes the `JsonValue` type itself, so that `Jason.Decode` and
`Jason.Encode` can use it, its wrapper `Value` type (which prevents
users of this package from reaching into `JsonValue`) as well as a `decoder`
and an `encode` function for the mentioned usage with the regular
`Json.Decode.decodeString` and `Json.Encode.encode` functions.

-}

import Array exposing (Array)
import Json.Decode
import Json.Encode


{-| A model for any JSON structure. A JSON value can be…
-}
type JsonValue
    = JsonString String -- … a JSON string, represented as an Elm String,
    | JsonNumber Float -- … a JSON number, represented as an Elm Float,
    | JsonBool Bool -- … a JSON boolean, represented as an Elm Bool,
    | JsonNull -- a JSON null (which does not need any extra data),
      -- … or arrays or objects, containing … more JSON values! Note that
      -- an array may contain mixed values (such as `[1, true, "string"]`),
      -- not just a single type. Same thing for objects, which are represented
      -- as `List`s of key-value pairs rather than records since we can’t know
      -- what fields to expect. (A `Dict` isn’t used to, because the order of
      -- the fields need to be preserved for full elm/json compatibility.)
    | JsonArray (Array JsonValue)
    | JsonObject (List ( String, JsonValue ))
      -- `undefined` is not part of JSON, but is defined here to due to a quirk
      -- in the `index` decoder.
    | CompatUndefined


{-| Both `Jason.Decode` and `Jason.Encode` expose an alias of this type.
But they don’t expose the `Value` constructor, so that nobody else can
reach into `JsonValue`. elm/json also ships with both `Json.Decode.Value`
and `Json.Encode.Value` – but the former is actually just an alias for
the latter.

I think the reason for hiding the JSON values in this opaque `Value` type is
not to encourage people to reach into the JSON manually. That’s an
anti-pattern. It’s better to decode the JSON into data types that are easier
to work with.

Since all JSON objects are represented as `List`s of key-value pairs, you
first have to try to find what you’re looking for in the list, and then
you’ll end up with `Maybe`s for the values. And you’d have to use a `case of`
on the values you get out, because you won’t know if it is a string, float,
boolean or whatever.

-}
type Value
    = Value JsonValue


{-| This decoder pretty much always succeeds, no matter what input you
give to it, since it decodes into such a general data structure that
closely models raw JSON.
-}
decoder : Json.Decode.Decoder JsonValue
decoder =
    Json.Decode.oneOf
        [ Json.Decode.map JsonString Json.Decode.string
        , Json.Decode.map JsonNumber Json.Decode.float
        , Json.Decode.map JsonBool Json.Decode.bool
        , Json.Decode.null JsonNull
        , Json.Decode.map JsonArray (Json.Decode.array decoderLazy)
        , Json.Decode.map JsonObject (Json.Decode.keyValuePairs decoderLazy)
        ]


decoderLazy : Json.Decode.Decoder JsonValue
decoderLazy =
    Json.Decode.lazy (\() -> decoder)


encode : JsonValue -> Json.Encode.Value
encode jsonValue =
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
            Json.Encode.array encode arr

        JsonObject pairs ->
            Json.Encode.object (List.map (Tuple.mapSecond encode) pairs)

        CompatUndefined ->
            -- This should never be reached – see the `index` decoder.
            Json.Encode.null


unwrap : Value -> JsonValue
unwrap (Value jsonValue) =
    jsonValue
