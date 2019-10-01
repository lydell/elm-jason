module Jason.Decode exposing (Decoder, andMap, at, bool, dict, fail, failRaw, field, float, fromCoreDecoder, fromMaybe, fromResult, index, int, lazy, list, map, map2, map3, map4, map5, map6, map7, map8, map9, nullable, oneOf, oneOrMore, optionalField, rawField, string, succeed, toCoreDecoder, tuple, tuple3, withDefault)

import Array exposing (Array)
import Dict exposing (Dict)
import Jason.Error exposing (Error(..))
import Jason.Value exposing (JsonValue(..))
import Json.Decode


{-| A Decoder is a function that takes a JSON value and returns a result.
The result can be either some Elm data type that is easier to work with
than a `JsonValue`, or an error.
-}
type alias Decoder a =
    JsonValue -> Result Error a


{-| Like `Json.Decode.string`.
-}
string : Decoder String
string jsonValue =
    case jsonValue of
        JsonString str ->
            Ok str

        _ ->
            Err (UnexpectedJsonValue { expected = JsonString "", actual = jsonValue })


{-| Like `Json.Decode.bool`.
-}
bool : Decoder Bool
bool jsonValue =
    case jsonValue of
        JsonBool boolean ->
            Ok boolean

        _ ->
            Err (UnexpectedJsonValue { expected = JsonBool False, actual = jsonValue })


{-| Like `Json.Decode.float`.
-}
float : Decoder Float
float jsonValue =
    case jsonValue of
        JsonNumber num ->
            Ok num

        _ ->
            Err (UnexpectedJsonValue { expected = JsonNumber 1.5, actual = jsonValue })


{-| Like `Json.Decode.int`.
-}
int : Decoder Int
int jsonValue =
    case jsonValue of
        JsonNumber num ->
            let
                floored =
                    floor num
            in
            if not (isInfinite num) && not (isNaN num) && toFloat floored == num then
                Ok floored

            else
                Err (UnexpectedJsonValue { expected = JsonNumber 1, actual = jsonValue })

        _ ->
            Err (UnexpectedJsonValue { expected = JsonNumber 1, actual = jsonValue })


{-| Like `Json.Decode.list`.

Note: There is no `array` decoder. You can map lists to arrays like so:

    list string |> map Array.fromList

-}
list : Decoder a -> Decoder (List a)
list decoder jsonValue =
    case jsonValue of
        JsonArray arr ->
            listHelper decoder 0 arr (Ok [])

        _ ->
            Err (UnexpectedJsonValue { expected = JsonArray Array.empty, actual = jsonValue })


listHelper : Decoder a -> Int -> Array JsonValue -> Result Error (List a) -> Result Error (List a)
listHelper decoder i arr acc =
    case Array.get i arr of
        Nothing ->
            Result.map List.reverse acc

        Just jsonValue ->
            case decoder jsonValue of
                Ok value ->
                    listHelper
                        decoder
                        (i + 1)
                        arr
                        (Result.map ((::) value) acc)

                Err error ->
                    Err (ErrorAtIndex { index = i, error = error })


{-| Like `Json.Decode.dict`.

You can get only the keys or values like so:

    dict string |> map Dict.keys

    dict string |> map Dict.values

-}
dict : Decoder a -> Decoder (Dict String a)
dict decoder jsonValue =
    case jsonValue of
        JsonObject dictionary ->
            dictHelper decoder (Dict.toList dictionary) (Ok Dict.empty)

        _ ->
            Err (UnexpectedJsonValue { expected = JsonObject Dict.empty, actual = jsonValue })


dictHelper : Decoder a -> List ( String, JsonValue ) -> Result Error (Dict String a) -> Result Error (Dict String a)
dictHelper decoder pairs acc =
    case pairs of
        [] ->
            acc

        ( fieldName, first ) :: rest ->
            case decoder first of
                Ok value ->
                    dictHelper
                        decoder
                        rest
                        (Result.map (Dict.insert fieldName value) acc)

                Err error ->
                    Err (ErrorAtField { field = fieldName, error = error })


{-| Like `Json.Decode.oneOrMore`.
-}
oneOrMore : (a -> List a -> b) -> Decoder a -> Decoder b
oneOrMore f decoder =
    list decoder
        |> andThen
            (\items _ ->
                case items of
                    [] ->
                        Err OneOrMoreEmptyList

                    first :: rest ->
                        Ok (f first rest)
            )


{-| Like `Json.Decode.field`.

This requires the field to be present in the object.

-}
field : String -> Decoder a -> Decoder a
field fieldName decoder jsonValue =
    case jsonValue of
        JsonObject dictionary ->
            case Dict.get fieldName dictionary of
                Just jsonValue2 ->
                    case decoder jsonValue2 of
                        Ok value ->
                            Ok value

                        Err error ->
                            Err (ErrorAtField { field = fieldName, error = error })

                Nothing ->
                    Err (MissingField { field = fieldName, dict = dictionary })

        _ ->
            Err (UnexpectedJsonValue { expected = JsonObject Dict.empty, actual = jsonValue })


{-| Like `field`, but returns `Nothing` if the field is missing or set to `null`.

Note that this differs from `Json.Decode.Extra.optionalField`. That function
only returns `Nothing` if the field is missing. `null` values are still
passed to your decoder.

This package treats missing fields and fields set to `null` the same, because in
practice I’ve found that to be the most useful. Sometimes APIs omit fields, sometimes
they set them to `null`. Sometimes they’re not even consistent.

-}
optionalField : String -> Decoder a -> Decoder (Maybe a)
optionalField fieldName decoder =
    rawField fieldName
        (\jsonValue ->
            case jsonValue of
                Missing ->
                    Ok Nothing

                JsonNull ->
                    Ok Nothing

                _ ->
                    case decoder jsonValue of
                        Ok value ->
                            Ok (Just value)

                        Err error ->
                            Err (ErrorAtField { field = fieldName, error = error })
        )


{-| Like `field`, but instead of failing on missing fields it passes `Missing`
to your decoder. This lets you distinguish between missing fields and fields
set to `null`, should you ever need to.
-}
rawField : String -> Decoder a -> Decoder a
rawField fieldName decoder jsonValue =
    case jsonValue of
        JsonObject dictionary ->
            decoder (Dict.get fieldName dictionary |> Maybe.withDefault Missing)

        _ ->
            Err (UnexpectedJsonValue { expected = JsonObject Dict.empty, actual = jsonValue })


{-| Like `Json.Decode.index`.
-}
index : Int -> Decoder a -> Decoder a
index i decoder jsonValue =
    case jsonValue of
        JsonArray arr ->
            case Array.get i arr of
                Just jsonValue2 ->
                    case decoder jsonValue2 of
                        Ok value ->
                            Ok value

                        Err error ->
                            Err (ErrorAtIndex { index = i, error = error })

                Nothing ->
                    Err (IndexOutOfBounds { index = i, array = arr })

        _ ->
            Err (UnexpectedJsonValue { expected = JsonArray Array.empty, actual = jsonValue })


{-| Like `Json.Decode.at`.
-}
at : List String -> Decoder a -> Decoder a
at fields decoder =
    List.foldr field decoder fields


{-| Decodes a JSON array with at least two items into an Elm Tuple with two items.
-}
tuple : ( Decoder a, Decoder b ) -> Decoder ( a, b )
tuple ( decoder1, decoder2 ) =
    map2
        Tuple.pair
        (index 0 decoder1)
        (index 1 decoder2)


{-| Decodes a JSON array with at least three items into an Elm Tuple with three items.
-}
tuple3 : ( Decoder a, Decoder b, Decoder c ) -> Decoder ( a, b, c )
tuple3 ( decoder1, decoder2, decoder3 ) =
    map3
        (\a b c -> ( a, b, c ))
        (index 0 decoder1)
        (index 1 decoder2)
        (index 2 decoder3)


{-| Like `Json.Decode.nullable`.
-}
nullable : Decoder a -> Decoder (Maybe a)
nullable decoder jsonValue =
    case jsonValue of
        JsonNull ->
            Ok Nothing

        _ ->
            map Just decoder jsonValue


{-| A shortcut for calling `Maybe.withDefault` inside a decoder.

    optionalField "age" int |> withDefault 0

Note that this differs from `Json.Decode.Extra.withDefault`. That function
swallows errors by returning the default value instead.

-}
withDefault : a -> Decoder (Maybe a) -> Decoder a
withDefault =
    map << Maybe.withDefault


{-| Like `Json.Decode.oneOf`.
-}
oneOf : List (Decoder a) -> Decoder a
oneOf decoders jsonValue =
    oneOfHelper decoders jsonValue (Err [])
        |> Result.mapError OneOfErrors


oneOfHelper : List (Decoder a) -> JsonValue -> Result (List Error) a -> Result (List Error) a
oneOfHelper decoders jsonValue acc =
    case decoders of
        [] ->
            Result.mapError List.reverse acc

        first :: rest ->
            case first jsonValue of
                Ok value ->
                    Ok value

                Err error ->
                    oneOfHelper rest jsonValue (Result.mapError ((::) error) acc)


{-| Like `Json.Decode.map`.
-}
map : (a -> b) -> Decoder a -> Decoder b
map f decoder jsonValue =
    decoder jsonValue |> Result.map f


{-| Like `Json.Decode.map2`.
-}
map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 f decoder1 decoder2 =
    decoder1 |> andThen (\a -> map (f a) decoder2)


{-| Like `Json.Decode.map3`.
-}
map3 : (a -> b -> c -> d) -> Decoder a -> Decoder b -> Decoder c -> Decoder d
map3 f decoder1 decoder2 decoder3 =
    decoder1 |> andThen (\a -> map2 (f a) decoder2 decoder3)


{-| Like `Json.Decode.map4`.
-}
map4 : (a -> b -> c -> d -> e) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e
map4 f decoder1 decoder2 decoder3 decoder4 =
    decoder1 |> andThen (\a -> map3 (f a) decoder2 decoder3 decoder4)


{-| Like `Json.Decode.map5`.
-}
map5 : (a -> b -> c -> d -> e -> f) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f
map5 f decoder1 decoder2 decoder3 decoder4 decoder5 =
    decoder1 |> andThen (\a -> map4 (f a) decoder2 decoder3 decoder4 decoder5)


{-| Like `Json.Decode.map6`.
-}
map6 : (a -> b -> c -> d -> e -> f -> g) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g
map6 f decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 =
    decoder1 |> andThen (\a -> map5 (f a) decoder2 decoder3 decoder4 decoder5 decoder6)


{-| Like `Json.Decode.map7`.
-}
map7 : (a -> b -> c -> d -> e -> f -> g -> h) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h
map7 f decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 =
    decoder1 |> andThen (\a -> map6 (f a) decoder2 decoder3 decoder4 decoder5 decoder6 decoder7)


{-| Like `Json.Decode.map8`.
-}
map8 : (a -> b -> c -> d -> e -> f -> g -> h -> i) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h -> Decoder i
map8 f decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8 =
    decoder1 |> andThen (\a -> map7 (f a) decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8)


{-| I felt like making a `map` for all single digit numbers :)
-}
map9 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h -> Decoder i -> Decoder j
map9 f decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8 decoder9 =
    decoder1 |> andThen (\a -> map8 (f a) decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8 decoder9)


{-| Like `Json.Decode.Extra.andMap`. Use it to decode objects with more than 9 fields.
-}
andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    map2 (|>)


{-| Like `Json.Decode.andThen`.
-}
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen f decoder jsonValue =
    case decoder jsonValue of
        Ok value ->
            f value jsonValue

        Err error ->
            Err error


{-| Like `Json.Decode.lazy`.
-}
lazy : (() -> Decoder a) -> Decoder a
lazy thunk =
    thunk ()


{-| Like `Json.Decode.succeed`.
-}
succeed : a -> Decoder a
succeed =
    always << Ok


{-| Like `Json.Decode.fail`.
-}
fail : String -> Decoder a
fail message jsonValue =
    Err (CustomError { message = message, jsonValue = jsonValue })


{-| Like `Json.Decode.fail`, except that it takes an `Error` instead of a `String`.
-}
failRaw : Error -> Decoder a
failRaw =
    always << Err


{-| Like `Json.Decode.Extra.fromResult`, except that it takes an `Error`
instead of a `String`. You can use `Result.mapError` to turn a Result
with some other error type into an `Error`.
-}
fromResult : Result Error a -> Decoder a
fromResult =
    always


{-| Like `Json.Decode.Extra.fromMaybe`, except that it takes an `Error` instead of a `String`.
-}
fromMaybe : Error -> Maybe a -> Decoder a
fromMaybe error =
    Result.fromMaybe error >> fromResult


{-| Turn our `Decoder` into a `Json.Decoder.Decoder`.
-}
toCoreDecoder : Decoder a -> Json.Decode.Decoder a
toCoreDecoder decoder =
    Json.Decode.value
        |> Json.Decode.andThen
            (\value ->
                case decoder (Jason.Value.fromCoreValue value) of
                    Ok result ->
                        Json.Decode.succeed result

                    Err error ->
                        Json.Decode.fail (Jason.Error.toString error)
            )


{-| Turn a `Json.Decoder.Decoder` into our `Decoder`.
-}
fromCoreDecoder : Json.Decode.Decoder a -> Decoder a
fromCoreDecoder coreDecoder jsonValue =
    case Json.Decode.decodeValue coreDecoder (Jason.Value.toCoreValue jsonValue) of
        Ok value ->
            Ok value

        Err error ->
            Err (Jason.Error.fromCoreError error)
