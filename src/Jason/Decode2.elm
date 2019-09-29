module Jason.Decode2 exposing (Decoder, andMap, at, bool, dict, fail, field, float, fromCoreDecoder, fromMaybe, fromResult, index, int, lazy, map, map2, map3, map4, map5, map6, map7, map8, map9, nullable, oneOf, oneOrMore, optionalField, string, succeed, toCoreDecoder, tuple, tuple3, withDefault)

import Array exposing (Array)
import Dict exposing (Dict)
import Jason.Error exposing (Error(..))
import Jason.Value exposing (JsonValue(..))
import Json.Decode


type alias Decoder a =
    JsonValue -> Result Error a


string : Decoder String
string jsonValue =
    case jsonValue of
        JsonString str ->
            Ok str

        _ ->
            Err (UnexpectedJsonValue { expected = JsonString "", actual = jsonValue })


bool : Decoder Bool
bool jsonValue =
    case jsonValue of
        JsonBool boolean ->
            Ok boolean

        _ ->
            Err (UnexpectedJsonValue { expected = JsonBool False, actual = jsonValue })


float : Decoder Float
float jsonValue =
    case jsonValue of
        JsonNumber num ->
            Ok num

        _ ->
            Err (UnexpectedJsonValue { expected = JsonNumber 0, actual = jsonValue })


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
                Err (CustomError ("Expected an Int, but got: " ++ String.fromFloat num))

        _ ->
            Err (UnexpectedJsonValue { expected = JsonNumber 0, actual = jsonValue })


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
                Ok val ->
                    listHelper
                        decoder
                        (i + 1)
                        arr
                        (Result.map ((::) val) acc)

                Err err ->
                    Err (ErrorAtIndex { index = i, error = err })


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

        ( key, first ) :: rest ->
            case decoder first of
                Ok val ->
                    dictHelper
                        decoder
                        rest
                        (Result.map (Dict.insert key val) acc)

                Err err ->
                    Err (ErrorAtKey { key = key, error = err })


oneOrMore : (a -> List a -> b) -> Decoder a -> Decoder b
oneOrMore f decoder =
    list decoder
        |> andThen
            (\items ->
                case items of
                    [] ->
                        fail (CustomError "Expected a list with at least one item, but got an empty list.")

                    first :: rest ->
                        succeed (f first rest)
            )


field : String -> Decoder a -> Decoder a
field key decoder jsonValue =
    case jsonValue of
        JsonObject dictionary ->
            case Dict.get key dictionary of
                Just jsonValue2 ->
                    case decoder jsonValue2 of
                        Ok val ->
                            Ok val

                        Err err ->
                            Err (ErrorAtKey { key = key, error = err })

                Nothing ->
                    Err (MissingKey { key = key, dict = dictionary })

        _ ->
            Err (UnexpectedJsonValue { expected = JsonObject (Dict.fromList []), actual = jsonValue })


optionalField : String -> Decoder a -> Decoder (Maybe a)
optionalField key decoder jsonValue =
    case jsonValue of
        JsonObject dictionary ->
            case Dict.get key dictionary |> Maybe.withDefault Missing of
                Missing ->
                    case decoder Missing of
                        Ok val ->
                            Ok (Just val)

                        Err _ ->
                            Ok Nothing

                JsonNull ->
                    case decoder JsonNull of
                        Ok val ->
                            Ok (Just val)

                        Err _ ->
                            Ok Nothing

                jsonValue2 ->
                    case decoder jsonValue2 of
                        Ok val ->
                            Ok (Just val)

                        Err err ->
                            Err (ErrorAtKey { key = key, error = err })

        _ ->
            Err (UnexpectedJsonValue { expected = JsonObject (Dict.fromList []), actual = jsonValue })


index : Int -> Decoder a -> Decoder a
index i decoder jsonValue =
    case jsonValue of
        JsonArray arr ->
            case Array.get i arr of
                Just jsonValue2 ->
                    case decoder jsonValue2 of
                        Ok val ->
                            Ok val

                        Err err ->
                            Err (ErrorAtIndex { index = i, error = err })

                Nothing ->
                    Err (IndexOutOfBounds { index = i, array = arr })

        _ ->
            Err (UnexpectedJsonValue { expected = JsonArray Array.empty, actual = jsonValue })


at : List String -> Decoder a -> Decoder a
at fields decoder =
    List.foldr field decoder fields


tuple : ( Decoder a, Decoder b ) -> Decoder ( a, b )
tuple ( decoder1, decoder2 ) =
    map2
        Tuple.pair
        (index 0 decoder1)
        (index 1 decoder2)


tuple3 : ( Decoder a, Decoder b, Decoder c ) -> Decoder ( a, b, c )
tuple3 ( decoder1, decoder2, decoder3 ) =
    map3
        (\a b c -> ( a, b, c ))
        (index 0 decoder1)
        (index 1 decoder2)
        (index 2 decoder3)


nullable : Decoder a -> Decoder (Maybe a)
nullable decoder jsonValue =
    case jsonValue of
        JsonNull ->
            Ok Nothing

        _ ->
            map Just decoder jsonValue


withDefault : a -> Decoder (Maybe a) -> Decoder a
withDefault val =
    map (Maybe.withDefault val)


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
                Ok val ->
                    Ok val

                Err err ->
                    oneOfHelper rest jsonValue (Result.mapError ((::) err) acc)


map : (a -> b) -> Decoder a -> Decoder b
map f decoder jsonValue =
    decoder jsonValue |> Result.map f


map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 f decoder1 decoder2 =
    decoder1 |> andThen (\a -> map (f a) decoder2)


map3 : (a -> b -> c -> d) -> Decoder a -> Decoder b -> Decoder c -> Decoder d
map3 f decoder1 decoder2 decoder3 =
    decoder1 |> andThen (\a -> map2 (f a) decoder2 decoder3)


map4 : (a -> b -> c -> d -> e) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e
map4 f decoder1 decoder2 decoder3 decoder4 =
    decoder1 |> andThen (\a -> map3 (f a) decoder2 decoder3 decoder4)


map5 : (a -> b -> c -> d -> e -> f) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f
map5 f decoder1 decoder2 decoder3 decoder4 decoder5 =
    decoder1 |> andThen (\a -> map4 (f a) decoder2 decoder3 decoder4 decoder5)


map6 : (a -> b -> c -> d -> e -> f -> g) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g
map6 f decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 =
    decoder1 |> andThen (\a -> map5 (f a) decoder2 decoder3 decoder4 decoder5 decoder6)


map7 : (a -> b -> c -> d -> e -> f -> g -> h) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h
map7 f decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 =
    decoder1 |> andThen (\a -> map6 (f a) decoder2 decoder3 decoder4 decoder5 decoder6 decoder7)


map8 : (a -> b -> c -> d -> e -> f -> g -> h -> i) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h -> Decoder i
map8 f decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8 =
    decoder1 |> andThen (\a -> map7 (f a) decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8)


map9 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h -> Decoder i -> Decoder j
map9 f decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8 decoder9 =
    decoder1 |> andThen (\a -> map8 (f a) decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8 decoder9)


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    map2 (|>)


andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen f decoder jsonValue =
    case decoder jsonValue of
        Ok val ->
            f val jsonValue

        Err err ->
            Err err


lazy : (() -> Decoder a) -> Decoder a
lazy thunk =
    andThen thunk (succeed ())


succeed : a -> Decoder a
succeed =
    always << Ok


fail : Error -> Decoder a
fail =
    always << Err


fromResult : Result Error a -> Decoder a
fromResult =
    always


fromMaybe : Error -> Maybe a -> Decoder a
fromMaybe error =
    Result.fromMaybe error >> fromResult


toCoreDecoder : Decoder a -> Json.Decode.Decoder a
toCoreDecoder decoder =
    Json.Decode.value
        |> Json.Decode.andThen
            (\val ->
                case decoder (Jason.Value.fromCoreValue val) of
                    Ok result ->
                        Json.Decode.succeed result

                    Err err ->
                        Json.Decode.fail (Jason.Error.toString err)
            )


fromCoreDecoder : Json.Decode.Decoder a -> Decoder a
fromCoreDecoder coreDecoder jsonValue =
    case Json.Decode.decodeValue coreDecoder (Jason.Value.toCoreValue jsonValue) of
        Ok val ->
            Ok val

        Err err ->
            Err (Jason.Error.fromCoreError err)
