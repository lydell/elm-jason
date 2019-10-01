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
    | MissingField { field : String, dict : Dict String JsonValue }
    | ErrorAtField { field : String, error : Error }
    | IndexOutOfBounds { index : Int, array : Array JsonValue }
    | ErrorAtIndex { index : Int, error : Error }
    | OneOfErrors (List Error)
    | OneOrMoreEmptyList
    | CustomError { message : String, jsonValue : JsonValue }


{-| Turns our `Error` into a `Json.Decode.Error`. The wording of the error
messages won’t be 100% the same as elm/json, but pretty close.
-}
toCoreError : Error -> Json.Decode.Error
toCoreError ourError =
    case ourError of
        UnexpectedJsonValue { expected, actual } ->
            Json.Decode.Failure (jsonTypeName expected) (Jason.Value.toCoreValue actual)

        MissingField { field, dict } ->
            Json.Decode.Failure ("an OBJECT with a field named `" ++ field ++ "`") (Jason.Value.toCoreValue (JsonObject dict))

        ErrorAtField { field, error } ->
            Json.Decode.Field field (toCoreError error)

        IndexOutOfBounds { index, array } ->
            Json.Decode.Failure ("a LONGER array. Need index " ++ String.fromInt index ++ " but only see " ++ String.fromInt (Array.length array) ++ " entries") (Jason.Value.toCoreValue (JsonArray array))

        ErrorAtIndex { index, error } ->
            Json.Decode.Index index (toCoreError error)

        OneOfErrors errors ->
            Json.Decode.OneOf (List.map toCoreError errors)

        OneOrMoreEmptyList ->
            Json.Decode.Failure "Expected a list with at least one item, but got an empty list." (Jason.Value.toCoreValue (JsonArray Array.empty))

        CustomError { message, jsonValue } ->
            Json.Decode.Failure message (Jason.Value.toCoreValue jsonValue)


{-| Turn a `Json.Decode.Error` into our `Error`.
-}
fromCoreError : Json.Decode.Error -> Error
fromCoreError coreError =
    case coreError of
        Json.Decode.Field field error ->
            ErrorAtField { field = field, error = fromCoreError error }

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
        withPath lines =
            let
                path =
                    pathListToString pathList
            in
            ("At " ++ path ++ ":\nError: " ++ String.join "\n" lines)
                |> indent

        preview =
            if shouldPreview then
                jsonPreview

            else
                jsonTypeName
    in
    case currentError of
        UnexpectedJsonValue { expected, actual } ->
            withPath
                [ "Expected " ++ jsonTypeName expected ++ "!"
                , "Value: " ++ preview actual
                ]

        MissingField { field, dict } ->
            let
                fields =
                    dict
                        |> Dict.keys
                        |> List.map jsonString
                        |> String.join ", "
            in
            withPath
                [ "This field does not exist!"
                , "Field: " ++ jsonString field
                , "Available fields: " ++ fields
                , "Value: " ++ preview (JsonObject dict)
                ]

        ErrorAtField { field, error } ->
            toStringHelper shouldPreview (Field field :: pathList) error

        IndexOutOfBounds { index, array } ->
            withPath
                [ "The index is out of bounds!"
                , "Index: " ++ String.fromInt index
                , "Length: " ++ String.fromInt (Array.length array)
                , "Value: " ++ preview (JsonArray array)
                ]

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
                    ("None of these " ++ String.fromInt numDecoders ++ " decoder choices succeeded:")
                        :: List.map ((++) "- " << toStringHelper shouldPreview pathList) errors
                        |> String.join "\n"
                        |> indent

        OneOrMoreEmptyList ->
            "Expected a list with at least one item, but got an empty list!"

        CustomError { message, jsonValue } ->
            withPath
                [ message
                , "Value: " ++ preview jsonValue
                ]


indent : String -> String
indent str =
    String.join "\n    " (String.split "\n" str)


jsonTypeName : JsonValue -> String
jsonTypeName jsonValue =
    case jsonValue of
        JsonString _ ->
            "a JSON string"

        JsonNumber num ->
            -- This is a little ugly, but does the trick…
            if toFloat (floor num) == num then
                "an integer JSON number"

            else
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
    = Field String
    | Index Int


pathToString : Path -> String
pathToString path =
    case path of
        Field field ->
            jsonString field

        Index index ->
            String.fromInt index


pathListToString : List Path -> String
pathListToString pathList =
    case List.reverse pathList of
        [] ->
            "top-level value"

        (first :: _) as fullPath ->
            let
                start =
                    case first of
                        Field _ ->
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
