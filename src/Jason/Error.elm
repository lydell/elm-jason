module Jason.Error exposing (Error(..), fromCoreError, toString)

import Array exposing (Array)
import Dict exposing (Dict)
import Jason.Value exposing (JsonValue(..))
import Json.Decode
import Json.Encode


type Error
    = UnexpectedJsonValue { expected : JsonValue, actual : JsonValue }
    | MissingKey { key : String, dict : Dict String JsonValue }
    | ErrorAtKey { key : String, error : Error }
    | IndexOutOfBounds { index : Int, array : Array JsonValue }
    | ErrorAtIndex { index : Int, error : Error }
    | OneOfErrors (List Error)
    | CustomError String


fromCoreError : Json.Decode.Error -> Error
fromCoreError coreError =
    case coreError of
        Json.Decode.Field key err ->
            ErrorAtKey { key = key, error = fromCoreError err }

        Json.Decode.Index i err ->
            ErrorAtIndex { index = i, error = fromCoreError err }

        Json.Decode.OneOf errors ->
            OneOfErrors (List.map fromCoreError errors)

        Json.Decode.Failure message coreValue ->
            UnexpectedJsonValue { expected = JsonString message, actual = Jason.Value.fromCoreValue coreValue }


toString : Error -> String
toString err =
    toStringHelper [] err


toStringHelper : List Path -> Error -> String
toStringHelper pathList err =
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
    case err of
        UnexpectedJsonValue { expected, actual } ->
            ("Expected " ++ jsonTypeName expected ++ " but got: " ++ jsonPreview actual)
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
            toStringHelper (Key key :: pathList) error

        IndexOutOfBounds { index, array } ->
            ("Index out of bounds. Index: " ++ String.fromInt index ++ ". Length: " ++ String.fromInt (Array.length array) ++ ".")
                |> withPath

        ErrorAtIndex { index, error } ->
            toStringHelper (Index index :: pathList) error

        OneOfErrors errors ->
            let
                numDecoders =
                    List.length errors

                s =
                    if numDecoders == 1 then
                        ""

                    else
                        "s"
            in
            if numDecoders == 0 then
                "Ran into a Jason.Decode.oneOf with an empty list of decoders!"

            else
                (String.fromInt numDecoders ++ " decoder" ++ s ++ " failed:")
                    :: List.map ((++) "- " << toStringHelper pathList) errors
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
