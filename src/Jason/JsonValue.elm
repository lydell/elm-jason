module Jason.JsonValue exposing (JsonValue(..), Value(..), decoder)

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode


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
      -- as `Dict`s rather than records since we can’t know what fields to expect.
    | JsonArray (Array JsonValue)
    | JsonObject (Dict String JsonValue)
      -- `undefined` is not part of JSON, but is defined here to due to a quirk
      -- in the `index` decoder.
    | CompatUndefined


{-| Both `Jason.Decode` and `Jason.Encode` expose an alias of this type.
But they don’t expose the `Value` constructor, so that nobody else can
reach into `JsonValue`. elm/json also ships with both `Json.Decode.Value`
and `Json.Encode.Value` – but the former is actually just an alias for
the latter.
-}
type Value
    = Value JsonValue


{-| This decoder pretty much always succeeds, no matter what input you
give to it, since it decodes into such a general data structure that
closely models raw JSON. In other words, this decoder turns an JSON
structure into an Elm value, but that Elm value is really difficult to
use since all objects are represented as `Dict`s, which give you `Maybe`s
when trying to take things out of them. And you’d have to use a `case of`
on the values you do get out, because you won’t know if it is a string,
float, boolean or whatever. So `Jason.Decode` then decodes the `JsonValue`s
produces by this decoder further into better data types.
-}
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
