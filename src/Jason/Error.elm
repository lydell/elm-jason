module Jason.Error exposing (Error(..), fromCoreError, toCoreError, toString, toStringSensitive)

import Array exposing (Array)
import Dict exposing (Dict)
import Jason.Value exposing (JsonValue(..))
import Json.Decode
import Json.Encode


{-| These are the kinds of errors that decoders can produce. When making your
own errors, you probably want to use `CustomError`.
-}
type Error
    = UnexpectedJsonValue { expected : JsonValue, actual : JsonValue }
    | MissingKey { key : String, dict : Dict String JsonValue }
    | ErrorAtKey { key : String, error : Error }
    | IndexOutOfBounds { index : Int, array : Array JsonValue }
    | ErrorAtIndex { index : Int, error : Error }
    | OneOfErrors (List Error)
    | CustomError String


{-| Turns our `Error` into a `Json.Decode.Error`. The wording of the error
messages won’t be 100% the same as elm/json, but pretty close.
-}
toCoreError : Error -> Json.Decode.Error
toCoreError ourError =
    case ourError of
        UnexpectedJsonValue { expected, actual } ->
            Json.Decode.Failure (jsonTypeName expected) (Jason.Value.toCoreValue actual)

        MissingKey { key, dict } ->
            Json.Decode.Failure ("an OBJECT with a field named `" ++ key ++ "`") (Jason.Value.toCoreValue (JsonObject dict))

        ErrorAtKey { key, error } ->
            Json.Decode.Field key (toCoreError error)

        IndexOutOfBounds { index, array } ->
            Json.Decode.Failure ("a LONGER array. Need index " ++ String.fromInt index ++ " but only see " ++ String.fromInt (Array.length array) ++ " entries") (Jason.Value.toCoreValue (JsonArray array))

        ErrorAtIndex { index, error } ->
            Json.Decode.Index index (toCoreError error)

        OneOfErrors errors ->
            Json.Decode.OneOf (List.map toCoreError errors)

        CustomError message ->
            Json.Decode.Failure message (Jason.Value.toCoreValue JsonNull)


{-| Turn a `Json.Decode.Error` into our `Error`.
-}
fromCoreError : Json.Decode.Error -> Error
fromCoreError coreError =
    case coreError of
        Json.Decode.Field key error ->
            ErrorAtKey { key = key, error = fromCoreError error }

        Json.Decode.Index i error ->
            ErrorAtIndex { index = i, error = fromCoreError error }

        Json.Decode.OneOf errors ->
            OneOfErrors (List.map fromCoreError errors)

        Json.Decode.Failure message coreValue ->
            UnexpectedJsonValue { expected = JsonString message, actual = Jason.Value.fromCoreValue coreValue }


{-| Turn an `Error` into a string. You can make your own function that
pattern matches on `Error` and views errors however you like.
-}
toString : Error -> String
toString error =
    toStringHelper True [] error


{-| Like `toString`, but only shows the type of erraneous values, rather
than a preview of the value itself. If you deal with sensitive data you
might not want that to appear in error messages.
-}
toStringSensitive : Error -> String
toStringSensitive error =
    toStringHelper False [] error


toStringHelper : Bool -> List Path -> Error -> String
toStringHelper shouldPreview pathList currentError =
    let
        withPath str =
            let
                path =
                    pathListToString pathList

                separator =
                    if String.isEmpty path then
                        ""

                    else
                        ":\n"
            in
            path ++ separator ++ str |> indent
    in
    case currentError of
        UnexpectedJsonValue { expected, actual } ->
            let
                actualString =
                    if shouldPreview then
                        jsonPreview actual

                    else
                        jsonTypeName actual
            in
            ("Expected " ++ jsonTypeName expected ++ " but got: " ++ actualString)
                |> withPath

        MissingKey { key, dict } ->
            let
                keys =
                    dict
                        |> Dict.keys
                        |> List.map jsonString
                        |> String.join ", "
            in
            ("This key does not exist: " ++ jsonString key ++ ". Available keys: " ++ keys ++ ".")
                |> withPath

        ErrorAtKey { key, error } ->
            toStringHelper shouldPreview (Key key :: pathList) error

        IndexOutOfBounds { index, array } ->
            ("Index out of bounds. Index: " ++ String.fromInt index ++ ". Length: " ++ String.fromInt (Array.length array) ++ ".")
                |> withPath

        ErrorAtIndex { index, error } ->
            toStringHelper shouldPreview (Index index :: pathList) error

        OneOfErrors errors ->
            case errors of
                [] ->
                    "Ran into a Jason.Decode.oneOf with an empty list of decoders!"

                [ error ] ->
                    toStringHelper shouldPreview pathList error

                _ ->
                    let
                        numDecoders =
                            List.length errors
                    in
                    (String.fromInt numDecoders ++ " decoder choices" ++ " failed:")
                        :: List.map ((++) "- " << toStringHelper shouldPreview pathList) errors
                        |> String.join "\n"
                        |> indent

        CustomError message ->
            withPath message


indent : String -> String
indent str =
    String.join "\n    " (String.split "\n" str)


jsonTypeName : JsonValue -> String
jsonTypeName jsonValue =
    case jsonValue of
        JsonString _ ->
            "a JSON string"

        JsonNumber _ ->
            "a JSON number"

        JsonBool _ ->
            "a JSON bool"

        JsonNull ->
            "a JSON null"

        JsonArray _ ->
            "a JSON array"

        JsonObject _ ->
            "a JSON object"

        Missing ->
            "no value"


type Path
    = Key String
    | Index Int


pathToString : Path -> String
pathToString path =
    case path of
        Key key ->
            jsonString key

        Index index ->
            String.fromInt index


pathListToString : List Path -> String
pathListToString pathList =
    case List.reverse pathList of
        [] ->
            ""

        (first :: _) as fullPath ->
            let
                start =
                    case first of
                        Key _ ->
                            "object"

                        Index _ ->
                            "array"

                end =
                    fullPath
                        |> List.map
                            (\path ->
                                "[" ++ pathToString path ++ "]"
                            )
                        |> String.join ""
            in
            start ++ end


jsonString : String -> String
jsonString =
    Json.Encode.encode 0 << Json.Encode.string


jsonPreview : JsonValue -> String
jsonPreview jsonValue =
    let
        maxLength =
            100

        half =
            maxLength // 2

        str =
            Jason.Value.toString 0 jsonValue
    in
    if String.length str > maxLength then
        String.left half str ++ "…" ++ String.right half str

    else
        str
