module Encode exposing (suite)

import Dict
import Expect exposing (Expectation)
import Fuzz
import Jason.Encode
import Json.Encode
import Set
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3)


verifySameOutput : Jason.Encode.Value -> Json.Encode.Value -> Int -> Expectation
verifySameOutput jasonValue jsonValue indentLevel =
    let
        jasonEncoded : String
        jasonEncoded =
            Jason.Encode.encode indentLevel jasonValue

        jsonEncoded : String
        jsonEncoded =
            Json.Encode.encode indentLevel jsonValue
    in
    jasonEncoded
        |> Expect.equal jsonEncoded


suite : Test
suite =
    describe "Jason.Encode"
        [ describe "Primitives"
            [ fuzz2 Fuzz.string Fuzz.int "string" <|
                \str ->
                    verifySameOutput
                        (Jason.Encode.string str)
                        (Json.Encode.string str)
            , fuzz2 Fuzz.int Fuzz.int "int" <|
                \num ->
                    verifySameOutput
                        (Jason.Encode.int num)
                        (Json.Encode.int num)
            , fuzz2 Fuzz.float Fuzz.int "float" <|
                \num ->
                    verifySameOutput
                        (Jason.Encode.float num)
                        (Json.Encode.float num)
            , fuzz2 Fuzz.bool Fuzz.int "bool" <|
                \boolean ->
                    verifySameOutput
                        (Jason.Encode.bool boolean)
                        (Json.Encode.bool boolean)
            , fuzz Fuzz.int "null" <|
                verifySameOutput
                    Jason.Encode.null
                    Json.Encode.null
            ]
        , describe "Arrays"
            [ fuzz2 (Fuzz.list Fuzz.string) Fuzz.int "list" <|
                \items ->
                    verifySameOutput
                        (Jason.Encode.list Jason.Encode.string items)
                        (Json.Encode.list Json.Encode.string items)
            , fuzz2 (Fuzz.array Fuzz.int) Fuzz.int "array" <|
                \items ->
                    verifySameOutput
                        (Jason.Encode.array Jason.Encode.int items)
                        (Json.Encode.array Json.Encode.int items)
            , fuzz2 (Fuzz.map Set.fromList (Fuzz.list Fuzz.float)) Fuzz.int "set" <|
                \items ->
                    verifySameOutput
                        (Jason.Encode.set Jason.Encode.float items)
                        (Json.Encode.set Json.Encode.float items)
            ]
        , describe "Objects"
            [ fuzz3
                (Fuzz.intRange 0 5)
                (Fuzz.tuple3 ( Fuzz.string, Fuzz.string, Fuzz.string ))
                Fuzz.int
                "object"
              <|
                \length ( key1, key2, key3 ) ->
                    verifySameOutput
                        (Jason.Encode.object
                            (List.take length
                                [ ( key1, Jason.Encode.string key2 )
                                , ( key2, Jason.Encode.int length )
                                , ( key3, Jason.Encode.bool (length <= 2) )
                                ]
                            )
                        )
                        (Json.Encode.object
                            (List.take length
                                [ ( key1, Json.Encode.string key2 )
                                , ( key2, Json.Encode.int length )
                                , ( key3, Json.Encode.bool (length <= 2) )
                                ]
                            )
                        )
            , fuzz2
                (Fuzz.list (Fuzz.tuple ( Fuzz.int, Fuzz.int )))
                Fuzz.int
                "dict"
              <|
                \items ->
                    let
                        dictionary =
                            Dict.fromList items

                        toKey i =
                            case i of
                                0 ->
                                    "zero"

                                1 ->
                                    "one"

                                _ ->
                                    String.fromInt i

                        toJasonValue i =
                            case i of
                                0 ->
                                    Jason.Encode.list Jason.Encode.bool [ True, False ]

                                1 ->
                                    Jason.Encode.float (toFloat i + 0.5)

                                _ ->
                                    Jason.Encode.string (String.fromInt i)

                        toJsonValue i =
                            case i of
                                0 ->
                                    Json.Encode.list Json.Encode.bool [ True, False ]

                                1 ->
                                    Json.Encode.float (toFloat i + 0.5)

                                _ ->
                                    Json.Encode.string (String.fromInt i)
                    in
                    verifySameOutput
                        (Jason.Encode.dict toKey toJasonValue dictionary)
                        (Json.Encode.dict toKey toJsonValue dictionary)
            ]
        ]
