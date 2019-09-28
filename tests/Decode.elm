module Decode exposing (suite)

-- import Dict
-- import Array

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Jason.Decode
import Jason.Encode
import Jason.JsonValue as JsonValue exposing (JsonValue(..))
import Json.Decode
import Test exposing (Test, describe, fuzz, fuzz2)


jsonFuzzer : Fuzzer Jason.Decode.Value
jsonFuzzer =
    Fuzz.oneOf
        [ Fuzz.map JsonString Fuzz.string
        , Fuzz.map JsonNumber Fuzz.float
        , Fuzz.map JsonBool Fuzz.bool
        , Fuzz.constant JsonNull

        -- , Fuzz.constant (JsonArray (Array.fromList []))
        -- , Fuzz.constant (JsonObject (Dict.fromList []))
        ]
        |> Fuzz.map JsonValue.Value


verifySameResult : Jason.Decode.Decoder a -> Json.Decode.Decoder a -> Jason.Decode.Value -> Expectation
verifySameResult jasonDecoder jsonDecoder val =
    let
        encoded : String
        encoded =
            Jason.Encode.encode 0 val

        jasonResult : Result String a
        jasonResult =
            Jason.Decode.decodeString jasonDecoder encoded
                |> Result.mapError Jason.Decode.errorToString

        jsonResult : Result String a
        jsonResult =
            Json.Decode.decodeString jsonDecoder encoded
                |> Result.mapError Json.Decode.errorToString
    in
    -- If we were very unlucky, `encoded` would always be a string with invalid
    -- JSON syntax. If so, this would never test the `Ok` case and _actually_
    -- compare that the two packages give the same result. But the "roundtrip"
    -- test case below should prove that we _do_ get `Ok`s.
    jasonResult
        |> Expect.equal jsonResult


suite : Test
suite =
    describe "Jason.Decode"
        [ fuzz jsonFuzzer "Roundtrip" <|
            \val ->
                let
                    encoded : String
                    encoded =
                        Jason.Encode.encode 0 val

                    jasonResult : Result Jason.Decode.Error Jason.Decode.Value
                    jasonResult =
                        Jason.Decode.decodeString Jason.Decode.value encoded
                in
                jasonResult
                    |> Expect.equal (Ok val)
        , describe "Primitives"
            [ fuzz jsonFuzzer "string" <|
                verifySameResult
                    Jason.Decode.string
                    Json.Decode.string
            , fuzz jsonFuzzer "bool" <|
                verifySameResult
                    Jason.Decode.bool
                    Json.Decode.bool
            , fuzz jsonFuzzer "int" <|
                verifySameResult
                    Jason.Decode.int
                    Json.Decode.int
            , fuzz jsonFuzzer "float" <|
                verifySameResult
                    Jason.Decode.float
                    Json.Decode.float
            ]
        , describe "Data Structures"
            [ fuzz jsonFuzzer "nullable" <|
                verifySameResult
                    (Jason.Decode.nullable Jason.Decode.string)
                    (Json.Decode.nullable Json.Decode.string)
            , fuzz jsonFuzzer "list" <|
                verifySameResult
                    (Jason.Decode.list Jason.Decode.bool)
                    (Json.Decode.list Json.Decode.bool)
            , fuzz jsonFuzzer "array" <|
                verifySameResult
                    (Jason.Decode.array Jason.Decode.int)
                    (Json.Decode.array Json.Decode.int)
            , fuzz jsonFuzzer "dict" <|
                verifySameResult
                    (Jason.Decode.dict Jason.Decode.float)
                    (Json.Decode.dict Json.Decode.float)
            , fuzz jsonFuzzer "keyValuePairs" <|
                verifySameResult
                    (Jason.Decode.keyValuePairs (Jason.Decode.nullable Jason.Decode.string))
                    (Json.Decode.keyValuePairs (Json.Decode.nullable Json.Decode.string))
            , fuzz (Fuzz.list Fuzz.int) "keyValuePairs keeps original order" <|
                \items ->
                    let
                        pairs =
                            List.indexedMap
                                (\i num -> ( "x" ++ String.fromInt i, num ))
                                items
                    in
                    pairs
                        |> List.map (Tuple.mapSecond Jason.Encode.int)
                        |> Jason.Encode.object
                        |> Jason.Decode.decodeValue (Jason.Decode.keyValuePairs Jason.Decode.int)
                        |> Expect.equal (Ok pairs)
            , fuzz jsonFuzzer "oneOrMore" <|
                verifySameResult
                    (Jason.Decode.oneOrMore (::) (Jason.Decode.list Jason.Decode.int))
                    (Json.Decode.oneOrMore (::) (Json.Decode.list Json.Decode.int))
            ]
        , describe "Object Primities"
            [ fuzz2 Fuzz.string jsonFuzzer "field" <|
                \key ->
                    verifySameResult
                        (Jason.Decode.field key (Jason.Decode.array Jason.Decode.float))
                        (Json.Decode.field key (Json.Decode.array Json.Decode.float))
            , fuzz2 (Fuzz.list Fuzz.string) jsonFuzzer "at" <|
                \fields ->
                    verifySameResult
                        (Jason.Decode.at fields (Jason.Decode.dict Jason.Decode.bool))
                        (Json.Decode.at fields (Json.Decode.dict Json.Decode.bool))
            , fuzz2 Fuzz.int jsonFuzzer "index" <|
                \i ->
                    verifySameResult
                        (Jason.Decode.index i (Jason.Decode.keyValuePairs (Jason.Decode.null ())))
                        (Json.Decode.index i (Json.Decode.keyValuePairs (Json.Decode.null ())))
            ]
        , describe "Inconsistent Structure"
            [ fuzz jsonFuzzer "maybe" <|
                verifySameResult
                    (Jason.Decode.maybe Jason.Decode.string)
                    (Json.Decode.maybe Json.Decode.string)
            , fuzz jsonFuzzer "maybe always succeeds" <|
                \val ->
                    Jason.Decode.decodeValue (Jason.Decode.maybe (Jason.Decode.fail "test")) val
                        |> Expect.ok
            , fuzz2 (Fuzz.intRange 0 3) jsonFuzzer "oneOf" <|
                \numDecoders ->
                    verifySameResult
                        (Jason.Decode.oneOf (List.repeat numDecoders Jason.Decode.int))
                        (Json.Decode.oneOf (List.repeat numDecoders Json.Decode.int))
            ]
        , describe "Run Decoders"
            [ -- These are also tested extensively in `verifySameResult`.
              fuzz Fuzz.string "decodeString + errorToString" <|
                \str ->
                    let
                        jasonResult : Result String String
                        jasonResult =
                            Jason.Decode.decodeString (Jason.Decode.succeed "test") str
                                |> Result.mapError Jason.Decode.errorToString

                        jsonResult : Result String String
                        jsonResult =
                            Json.Decode.decodeString (Json.Decode.succeed "test") str
                                |> Result.mapError Json.Decode.errorToString
                    in
                    jasonResult
                        |> Expect.equal jsonResult
            , fuzz jsonFuzzer "decodeValue" <|
                \val ->
                    let
                        jasonResult : Result String Int
                        jasonResult =
                            Jason.Decode.decodeValue Jason.Decode.int val
                                |> Result.mapError Jason.Decode.errorToString

                        jsonResult : Result String Int
                        jsonResult =
                            -- This uses a couple of internal functions to be
                            -- able to decode a Jason Value with Json (Jason
                            -- Values are turned into Json Values).
                            Json.Decode.decodeValue Json.Decode.int (JsonValue.encode (JsonValue.unwrap val))
                                |> Result.mapError Json.Decode.errorToString
                    in
                    jasonResult
                        |> Expect.equal jsonResult
            ]
        , describe "Mapping"
            [ fuzz jsonFuzzer "map" <|
                verifySameResult
                    (Jason.Decode.map String.toUpper Jason.Decode.string)
                    (Json.Decode.map String.toUpper Json.Decode.string)
            , fuzz jsonFuzzer "map2" <|
                verifySameResult
                    (Jason.Decode.map2 (+) Jason.Decode.int Jason.Decode.int)
                    (Json.Decode.map2 (+) Json.Decode.int Json.Decode.int)
            , fuzz jsonFuzzer "map3" <|
                let
                    collect a b c =
                        ( a, b, c )
                in
                \val ->
                    verifySameResult
                        (Jason.Decode.map3 collect Jason.Decode.string Jason.Decode.int Jason.Decode.float)
                        (Json.Decode.map3 collect Json.Decode.string Json.Decode.int Json.Decode.float)
                        val
            , fuzz jsonFuzzer "map4" <|
                let
                    collect a b c d =
                        ( a, b, ( c, d ) )
                in
                \val ->
                    verifySameResult
                        (Jason.Decode.map4 collect Jason.Decode.string Jason.Decode.int Jason.Decode.float Jason.Decode.bool)
                        (Json.Decode.map4 collect Json.Decode.string Json.Decode.int Json.Decode.float Json.Decode.bool)
                        val
            , fuzz jsonFuzzer "map5" <|
                let
                    collect a b c d e =
                        ( a, b, ( c, d, e ) )
                in
                \val ->
                    verifySameResult
                        (Jason.Decode.map5 collect Jason.Decode.string Jason.Decode.int Jason.Decode.float Jason.Decode.bool Jason.Decode.string)
                        (Json.Decode.map5 collect Json.Decode.string Json.Decode.int Json.Decode.float Json.Decode.bool Json.Decode.string)
                        val
            , fuzz jsonFuzzer "map6" <|
                let
                    collect a b c d e f =
                        ( a, b, ( c, d, ( e, f ) ) )
                in
                \val ->
                    verifySameResult
                        (Jason.Decode.map6 collect Jason.Decode.string Jason.Decode.int Jason.Decode.float Jason.Decode.bool Jason.Decode.string Jason.Decode.int)
                        (Json.Decode.map6 collect Json.Decode.string Json.Decode.int Json.Decode.float Json.Decode.bool Json.Decode.string Json.Decode.int)
                        val
            , fuzz jsonFuzzer "map7" <|
                let
                    collect a b c d e f g =
                        ( a, b, ( c, d, ( e, f, g ) ) )
                in
                \val ->
                    verifySameResult
                        (Jason.Decode.map7 collect Jason.Decode.string Jason.Decode.int Jason.Decode.float Jason.Decode.bool Jason.Decode.string Jason.Decode.int Jason.Decode.float)
                        (Json.Decode.map7 collect Json.Decode.string Json.Decode.int Json.Decode.float Json.Decode.bool Json.Decode.string Json.Decode.int Json.Decode.float)
                        val
            , fuzz jsonFuzzer "map8" <|
                let
                    collect a b c d e f g h =
                        ( a, b, ( c, d, ( e, f, ( g, h ) ) ) )
                in
                \val ->
                    verifySameResult
                        (Jason.Decode.map8 collect Jason.Decode.string Jason.Decode.int Jason.Decode.float Jason.Decode.bool Jason.Decode.string Jason.Decode.int Jason.Decode.float Jason.Decode.bool)
                        (Json.Decode.map8 collect Json.Decode.string Json.Decode.int Json.Decode.float Json.Decode.bool Json.Decode.string Json.Decode.int Json.Decode.float Json.Decode.bool)
                        val
            ]
        , describe "Fancy Decoding"
            [ fuzz jsonFuzzer "lazy" <|
                verifySameResult
                    (Jason.Decode.lazy (\() -> Jason.Decode.lazy (\() -> Jason.Decode.float)))
                    (Json.Decode.lazy (\() -> Json.Decode.lazy (\() -> Json.Decode.float)))
            , fuzz jsonFuzzer "value" <|
                verifySameResult
                    -- This uses a couple of internal functions to be able to compare the `Value`s
                    -- from Jason and Json (Jason Values are turned into Json Values).
                    (Jason.Decode.value |> Jason.Decode.map (JsonValue.unwrap >> JsonValue.encode))
                    Json.Decode.value
            , fuzz2 Fuzz.int jsonFuzzer "null" <|
                \num ->
                    verifySameResult
                        (Jason.Decode.null num)
                        (Json.Decode.null num)
            , fuzz2 Fuzz.float jsonFuzzer "succeed" <|
                \num ->
                    verifySameResult
                        (Jason.Decode.succeed num)
                        (Json.Decode.succeed num)
            , fuzz2 Fuzz.bool jsonFuzzer "succeed always succeeds" <|
                \boolean val ->
                    Jason.Decode.decodeValue (Jason.Decode.succeed boolean) val
                        |> Expect.ok
            , fuzz2 Fuzz.string jsonFuzzer "fail" <|
                \message ->
                    verifySameResult
                        (Jason.Decode.fail message)
                        (Json.Decode.fail message)
            , fuzz2 Fuzz.string jsonFuzzer "fail always fails" <|
                \message val ->
                    Jason.Decode.decodeValue (Jason.Decode.fail message) val
                        |> Expect.err
            , fuzz jsonFuzzer "andThen" <|
                verifySameResult
                    (Jason.Decode.index 0 Jason.Decode.int |> Jason.Decode.andThen (\i -> Jason.Decode.index i (Jason.Decode.maybe Jason.Decode.int)))
                    (Json.Decode.index 0 Json.Decode.int |> Json.Decode.andThen (\i -> Json.Decode.index i (Json.Decode.maybe Json.Decode.int)))
            , fuzz jsonFuzzer "andThen always fails if illogical" <|
                \val ->
                    Jason.Decode.decodeValue (Jason.Decode.int |> Jason.Decode.andThen (always Jason.Decode.string)) val
                        |> Expect.err
            ]
        ]
