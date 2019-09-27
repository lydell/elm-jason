module Jason.Decode exposing
    ( Decoder, string, bool, int, float
    , nullable, list, array, dict, keyValuePairs, oneOrMore
    , field, at, index
    , maybe, oneOf
    , decodeString, decodeValue, Value, Error(..), errorToString
    , map, map2, map3, map4, map5, map6, map7, map8
    , lazy, value, null, succeed, fail, andThen
    )

{-| Turn JSON values into Elm values. Definitely check out this [intro to
JSON decoders][guide] to get a feel for how this library works!
[guide]: <https://guide.elm-lang.org/effects/json.html>


# Primitives

@docs Decoder, string, bool, int, float


# Data Structures

@docs nullable, list, array, dict, keyValuePairs, oneOrMore


# Object Primitives

@docs field, at, index


# Inconsistent Structure

@docs maybe, oneOf


# Run Decoders

@docs decodeString, decodeValue, Value, Error, errorToString


# Mapping

**Note:** If you run out of map functions, take a look at [elm-json-decode-pipeline][pipe]
which makes it easier to handle large objects, but produces lower quality type
errors.
[pipe]: /packages/NoRedInk/elm-json-decode-pipeline/latest

@docs map, map2, map3, map4, map5, map6, map7, map8


# Fancy Decoding

@docs lazy, value, null, succeed, fail, andThen

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Jason.Encode
import Jason.JsonValue as JsonValue exposing (JsonValue(..))
import Json.Decode
import Json.Encode
import Set exposing (Set)



-- PRIMITIVES


{-| elm/json uses:

    type Decoder a
        = Decoder

Now, imagine we had done the same here. Let’s try to implement the `string` decoder.

    string : Decoder String
    string =
        xxx

What should `xxx` be? Well, the type annotation says that it has to be of the
Decoder type. How do we get such a value? There’s just one: `Decoder`.

    string : Decoder String
    string =
        Decoder

Now let’s do `bool`:

    bool : Decoder Bool
    bool =
        Decoder

How are we going to tell `string` and `bool` apart when running decoders?
They’re the same value!

elm/json gets away with this type since it’s implemented in JavaScript. But
for an Elm implementation, we need to attach more information to the
`Decoder` value. (That’s what elm/json is doing behind the scenes in
JavaScript.)

What kind of information should we attach? A function!

-}
type Decoder a
    = Decoder (DecoderFunction a)


{-| A Decoder is really just a function that takes a JSON value as input and
returns a Result.

This function is still wrapped in the above Decoder type so that others
cannot get to the function directly – the `Decoder` value is not exposed,
only the type.

-}
type alias DecoderFunction a =
    JsonValue -> Result Error a


{-| Implementing decoders consists of returning a function wrapped in `Decoder`.
Check if the input JSON value has the expected type. If so, return an Ok.
Otherwise, return an error.

How does elm/json do these things? It has JavaScript code that does checks like
`typeof jsonValue === "string"` and returns results based on that. The core
Elm libraries have to reach into the dirty JavaScript objects somewhere, since
after all we _are_ running in a JavaScript world.

-}
string : Decoder String
string =
    Decoder
        (\jsonValue ->
            case jsonValue of
                JsonString str ->
                    Ok str

                _ ->
                    failure "a STRING" jsonValue
        )


{-| -}
bool : Decoder Bool
bool =
    Decoder
        (\jsonValue ->
            case jsonValue of
                JsonBool boolean ->
                    Ok boolean

                _ ->
                    failure "a BOOL" jsonValue
        )


{-| The `int` decoder is a little bit tricker. JSON only has floats, so we have
to validate that the float does not have a fractional part, or is one of the
special float-only values Infinity, -Infinity and NaN.
-}
int : Decoder Int
int =
    Decoder
        (\jsonValue ->
            case jsonValue of
                JsonNumber num ->
                    let
                        truncated =
                            truncate num
                    in
                    -- This check feels redundant to me, but elm/json does this.
                    -- I think it might be for performance.
                    if -2147483647 < num && num < 2147483647 && toFloat truncated == num then
                        Ok truncated

                    else
                        let
                            rounded =
                                if num < 0 then
                                    ceiling num

                                else
                                    floor num
                        in
                        if not (isInfinite num) && not (isNaN num) && toFloat rounded == num then
                            Ok rounded

                        else
                            failure "an INT" jsonValue

                _ ->
                    failure "an INT" jsonValue
        )


{-| -}
float : Decoder Float
float =
    Decoder
        (\jsonValue ->
            case jsonValue of
                JsonNumber num ->
                    Ok num

                _ ->
                    failure "a FLOAT" jsonValue
        )



-- DATA STRUCTURES


{-| Copied straight from elm/json.
-}
nullable : Decoder a -> Decoder (Maybe a)
nullable decoder =
    oneOf
        [ null Nothing
        , map Just decoder
        ]


{-| Decoding a list consists of first checking that the JSON value is an array
and then running the passed decoder on each item in that array. If all of
that succeeds we return a list, otherwise we return the first error that occurred.
-}
list : Decoder a -> Decoder (List a)
list (Decoder decoderFunction) =
    Decoder
        (\jsonValue ->
            case jsonValue of
                JsonArray jsonArray ->
                    arrayHelp decoderFunction 0 jsonArray (Ok Array.empty)
                        |> Result.map Array.toList

                _ ->
                    failure "a LIST" jsonValue
        )


{-| The `array` decoder is almost identical to `list`. Actually, `list` is
implemented using the same helper function as `array` here.
-}
array : Decoder a -> Decoder (Array a)
array (Decoder decoderFunction) =
    Decoder
        (\jsonValue ->
            case jsonValue of
                JsonArray jsonArray ->
                    arrayHelp decoderFunction 0 jsonArray (Ok Array.empty)

                _ ->
                    failure "an ARRAY" jsonValue
        )


{-| This function is a little bit weird because it is written in a tail call recursive
way. But it runs a decoder function one each item, from left to right, and stops if
there’s an error.
-}
arrayHelp : DecoderFunction a -> Int -> Array JsonValue -> Result Error (Array a) -> Result Error (Array a)
arrayHelp decoderFunction i jsonArray acc =
    case Array.get i jsonArray of
        Nothing ->
            acc

        Just jsonValue ->
            case decoderFunction jsonValue of
                Ok val ->
                    arrayHelp
                        decoderFunction
                        (i + 1)
                        jsonArray
                        (Result.map (Array.push val) acc)

                Err err ->
                    Err (Index i err)


{-| The `dict` decoder is implemented very similarly to `array` and `list`.
After checking that the JSON input is an object, we need to check all the
values of it.
-}
dict : Decoder a -> Decoder (Dict String a)
dict (Decoder decoderFunction) =
    Decoder
        (\jsonValue ->
            case jsonValue of
                JsonObject dictionary ->
                    dictHelp decoderFunction (Dict.toList dictionary) (Ok Dict.empty)

                _ ->
                    failure "an OBJECT" jsonValue
        )


{-| Again, a tail call recursive helper function.
-}
dictHelp : DecoderFunction a -> List ( String, JsonValue ) -> Result Error (Dict String a) -> Result Error (Dict String a)
dictHelp decoderFunction pairs acc =
    case pairs of
        [] ->
            acc

        ( key, first ) :: rest ->
            case decoderFunction first of
                Ok val ->
                    dictHelp
                        decoderFunction
                        rest
                        (Result.map (Dict.insert key val) acc)

                Err err ->
                    Err (Field key err)


{-| -}
keyValuePairs : Decoder a -> Decoder (List ( String, a ))
keyValuePairs =
    dict >> map Dict.toList


{-| Copied straight from elm/json.
-}
oneOrMore : (a -> List a -> value) -> Decoder a -> Decoder value
oneOrMore toValue decoder =
    list decoder
        |> andThen (oneOrMoreHelp toValue)


{-| Copied straight from elm/json.
-}
oneOrMoreHelp : (a -> List a -> value) -> List a -> Decoder value
oneOrMoreHelp toValue xs =
    case xs of
        [] ->
            -- elm/json has this a/an typo.
            fail "a ARRAY with at least ONE element"

        y :: ys ->
            succeed (toValue y ys)



-- OBJECT PRIMITIVES


{-| The `field` decoder needs to check that the input JSON is an object,
that the given `key` exists and then finally run the passed decoder on the
value at `key`.
-}
field : String -> Decoder a -> Decoder a
field key (Decoder decoderFunction) =
    Decoder
        (\jsonValue ->
            let
                keyFailure =
                    failure ("an OBJECT with a field named `" ++ key ++ "`") jsonValue
            in
            case jsonValue of
                JsonObject dictionary ->
                    case Dict.get key dictionary of
                        Just jsonValue2 ->
                            case decoderFunction jsonValue2 of
                                Ok val ->
                                    Ok val

                                Err err ->
                                    Err (Field key err)

                        Nothing ->
                            keyFailure

                _ ->
                    keyFailure
        )


{-| Copied straight from elm/json.
-}
at : List String -> Decoder a -> Decoder a
at fields decoder =
    List.foldr field decoder fields


{-| The `index` decoder needs to check that the input JSON is an array,
and that the given `index` isn’t too large (outside the length of the array).
An extra complication here is replicating elm/json’s behavior for negative
indexes.
-}
index : Int -> Decoder a -> Decoder a
index i (Decoder decoderFunction) =
    Decoder
        (\jsonValue ->
            case jsonValue of
                JsonArray arr ->
                    case Array.get i arr of
                        Nothing ->
                            let
                                length =
                                    Array.length arr
                            in
                            if i < 0 then
                                -- elm/json tries to decode `array[-1]` etc for negative indexes,
                                -- which means decoding `undefined`.
                                case decoderFunction CompatUndefined of
                                    Ok _ ->
                                        -- There is no decoder function that deals with `CompatUndefined`,
                                        -- so it should be possible to get here.
                                        Err (Failure "Impossible state reached!" (JsonValue.Value jsonValue))

                                    Err err ->
                                        Err (Index i err)

                            else
                                failure ("a LONGER array. Need index " ++ String.fromInt i ++ " but only see " ++ String.fromInt length ++ " entries") jsonValue

                        Just item ->
                            case decoderFunction item of
                                Ok val ->
                                    Ok val

                                Err err ->
                                    Err (Index i err)

                _ ->
                    failure "an ARRAY" jsonValue
        )



-- WEIRD STRUCTURE


{-| Copied straight from elm/json.
-}
maybe : Decoder a -> Decoder (Maybe a)
maybe decoder =
    oneOf
        [ map Just decoder
        , succeed Nothing
        ]


{-| -}
oneOf : List (Decoder a) -> Decoder a
oneOf decoders =
    Decoder
        (\jsonValue ->
            -- `oneOfHelp` uses `::` to build the errors list, which means that
            -- it will end up backwards. So reverse it.
            oneOfHelp decoders jsonValue (Err [])
                |> Result.mapError (List.reverse >> OneOf)
        )


{-| Yet another tail call recursive helper function. It tries all passed decoders,
from left to right, and stops if one of them succeeds. If all of them fail, a list
of all errors is returned.

If the list of decoders is empty, an empty list of errors is returned. This is later
used to give an error message about this. As opposed to most (all?) other error
messages, this is not about something being wrong with the input JSON, but with the
decoder itself. A more “type safe” way to define `oneOf` could be:

    oneOf : Decoder a -> List (Decoder a) -> Decoder a

That way you _have_ to supply at least one decoder, but I’m not sure it matters in
practice. I had never even thought about what `oneOf []` would do before writing
this package.

-}
oneOfHelp : List (Decoder a) -> JsonValue -> Result (List Error) a -> Result (List Error) a
oneOfHelp decoders jsonValue acc =
    case decoders of
        [] ->
            acc

        (Decoder first) :: rest ->
            case first jsonValue of
                Ok val ->
                    Ok val

                Err err ->
                    oneOfHelp rest jsonValue (Result.mapError ((::) err) acc)



-- MAPPING


{-| -}
map : (a -> b) -> Decoder a -> Decoder b
map f (Decoder decoderFunction) =
    Decoder (decoderFunction >> Result.map f)


{-| -}
map2 : (a -> b -> value) -> Decoder a -> Decoder b -> Decoder value
map2 f decoder1 decoder2 =
    decoder1
        |> andThen
            (\val1 ->
                map (f val1) decoder2
            )


{-| -}
map3 : (a -> b -> c -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder value
map3 f decoder1 decoder2 decoder3 =
    decoder1
        |> andThen
            (\val1 ->
                map2 (f val1) decoder2 decoder3
            )


{-| -}
map4 : (a -> b -> c -> d -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder value
map4 f decoder1 decoder2 decoder3 decoder4 =
    decoder1
        |> andThen
            (\val1 ->
                map3 (f val1) decoder2 decoder3 decoder4
            )


{-| -}
map5 : (a -> b -> c -> d -> e -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder value
map5 f decoder1 decoder2 decoder3 decoder4 decoder5 =
    decoder1
        |> andThen
            (\val1 ->
                map4 (f val1) decoder2 decoder3 decoder4 decoder5
            )


{-| -}
map6 : (a -> b -> c -> d -> e -> f -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder value
map6 f decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 =
    decoder1
        |> andThen
            (\val1 ->
                map5 (f val1) decoder2 decoder3 decoder4 decoder5 decoder6
            )


{-| -}
map7 : (a -> b -> c -> d -> e -> f -> g -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder value
map7 f decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 =
    decoder1
        |> andThen
            (\val1 ->
                map6 (f val1) decoder2 decoder3 decoder4 decoder5 decoder6 decoder7
            )


{-| -}
map8 : (a -> b -> c -> d -> e -> f -> g -> h -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h -> Decoder value
map8 f decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8 =
    decoder1
        |> andThen
            (\val1 ->
                map7 (f val1) decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8
            )



-- RUN DECODERS


{-| I didn’t want to write a full JSON parser in Elm, so instead I use the
regular `Json.Decode.decodeString` to do the parsing for us.
See JsonValue.elm for more information.
-}
decodeString : Decoder a -> String -> Result Error a
decodeString (Decoder decoderFunction) str =
    case Json.Decode.decodeString JsonValue.decoder str of
        Ok jsonValue ->
            decoderFunction jsonValue

        Err err ->
            Err (convertError str err)


{-| `Json.Decode.Error` and `Error` in this package are identical, but still
technically different types, so we need to map the one to the other.
-}
convertError : String -> Json.Decode.Error -> Error
convertError jsonString err =
    case err of
        Json.Decode.Field str err2 ->
            Field str (convertError jsonString err2)

        Json.Decode.Index i err2 ->
            Index i (convertError jsonString err2)

        Json.Decode.OneOf errors ->
            OneOf (List.map (convertError jsonString) errors)

        Json.Decode.Failure message _ ->
            -- The most likely reason to get here is because of invalid JSON
            -- syntax. Just like elm/json, use the original JSON string input
            -- as the failure value. We can’t just pass along the underscored
            -- parameter above, because it is a `Json.Decode.Value` rather than
            -- a `Value` from this package. And we can’t convert it either,
            -- because the only way of doing that is by decoding the value with
            -- `Json.Decode` decoders, which can fail – and then we have the same
            -- problem again. In practice though, `JsonValue.decoder` (used in
            -- `decodeString` above) rarely (never?) fails, so we really only
            -- end up here due to invalid JSON syntax.
            Failure message (JsonValue.Value (JsonString jsonString))


{-| This function turned out surprisingly simple. All that needs to be
done is unwrapping the opaque Decoder and Value types, and then running
the unwrapped function with the unwrapped value.
-}
decodeValue : Decoder a -> Value -> Result Error a
decodeValue (Decoder decoderFunction) (JsonValue.Value jsonValue) =
    decoderFunction jsonValue


{-| -}
type alias Value =
    Jason.Encode.Value


{-| Copied straight from elm/json.
-}
type Error
    = Field String Error
    | Index Int Error
    | OneOf (List Error)
    | Failure String Value


{-| elm/json has this helper function too, but in JavaScript.
-}
failure : String -> JsonValue -> Result Error a
failure type_ jsonValue =
    Err (Failure ("Expecting " ++ type_) (JsonValue.Value jsonValue))


{-| Copied straight from elm/json.
-}
errorToString : Error -> String
errorToString error =
    errorToStringHelp error []


{-| Copied straight from elm/json, but with the special `CompatUndefined` check added.
-}
errorToStringHelp : Error -> List String -> String
errorToStringHelp error context =
    case error of
        Field f err ->
            let
                isSimple =
                    case String.uncons f of
                        Nothing ->
                            False

                        Just ( char, rest ) ->
                            Char.isAlpha char && String.all Char.isAlphaNum rest

                fieldName =
                    if isSimple then
                        "." ++ f

                    else
                        "['" ++ f ++ "']"
            in
            errorToStringHelp err (fieldName :: context)

        Index i err ->
            let
                indexName =
                    "[" ++ String.fromInt i ++ "]"
            in
            errorToStringHelp err (indexName :: context)

        OneOf errors ->
            case errors of
                [] ->
                    "Ran into a Json.Decode.oneOf with no possibilities"
                        ++ (case context of
                                [] ->
                                    "!"

                                _ ->
                                    " at json" ++ String.join "" (List.reverse context)
                           )

                [ err ] ->
                    errorToStringHelp err context

                _ ->
                    let
                        starter =
                            case context of
                                [] ->
                                    "Json.Decode.oneOf"

                                _ ->
                                    "The Json.Decode.oneOf at json" ++ String.join "" (List.reverse context)

                        introduction =
                            starter ++ " failed in the following " ++ String.fromInt (List.length errors) ++ " ways:"
                    in
                    String.join "\n\n" (introduction :: List.indexedMap errorOneOf errors)

        Failure msg json ->
            let
                introduction =
                    case context of
                        [] ->
                            "Problem with the given value:\n\n"

                        _ ->
                            "Problem with the value at json" ++ String.join "" (List.reverse context) ++ ":\n\n    "

                encoded =
                    -- This is the only time `CompatUndefined` is ever used. See
                    -- the `index` decoder for more information.
                    if json == JsonValue.Value CompatUndefined then
                        "undefined"

                    else
                        Jason.Encode.encode 4 json
            in
            introduction ++ indent encoded ++ "\n\n" ++ msg


{-| Copied straight from elm/json.
-}
errorOneOf : Int -> Error -> String
errorOneOf i error =
    "\n\n(" ++ String.fromInt (i + 1) ++ ") " ++ indent (errorToString error)


{-| Copied straight from elm/json.
-}
indent : String -> String
indent str =
    String.join "\n    " (String.split "\n" str)



-- FANCY PRIMITIVES


{-| -}
succeed : a -> Decoder a
succeed =
    Decoder << always << Ok


{-| -}
fail : String -> Decoder a
fail message =
    Decoder (Err << Failure message << JsonValue.Value)


{-| -}
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen f (Decoder decoderFunction) =
    Decoder
        (\jsonValue ->
            case decoderFunction jsonValue of
                Ok val ->
                    let
                        (Decoder decoderFunction2) =
                            f val
                    in
                    decoderFunction2 jsonValue

                Err err ->
                    Err err
        )


{-| Copied straight from elm/json.
-}
lazy : (() -> Decoder a) -> Decoder a
lazy thunk =
    andThen thunk (succeed ())


{-| -}
value : Decoder Value
value =
    Decoder (Ok << JsonValue.Value)


{-| -}
null : a -> Decoder a
null val =
    Decoder
        (\jsonValue ->
            case jsonValue of
                JsonNull ->
                    Ok val

                _ ->
                    failure "null" jsonValue
        )
