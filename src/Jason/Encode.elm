module Jason.Encode exposing
    ( encode, Value
    , string, int, float, bool, null
    , list, array, set
    , object, dict
    )

{-| Library for turning Elm values into Json values.


# Encoding

@docs encode, Value


# Primitives

@docs string, int, float, bool, null


# Arrays

@docs list, array, set


# Objects

@docs object, dict

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Jason.JsonValue as JsonValue exposing (JsonValue(..))
import Json.Encode
import Set exposing (Set)



-- ENCODE


{-| -}
type alias Value =
    JsonValue.Value


{-| I didn’t feel like implementing a full JSON stringifier in Elm,
so instead I use the regular `Json.Encode.encode`.

Encoding is all about turning Elm values into an internal representation
(the Value type), and then turning that internal representation into a
string (via this `encode`) function.

The point of the internal representation is that it can only contain
values that be turned into JSON. This forces you to convert your Elm
values into values that can be represented in JSON.

-}
encode : Int -> Value -> String
encode indentLevel (JsonValue.Value jsonValue) =
    Json.Encode.encode indentLevel (JsonValue.encode jsonValue)



-- PRIMITIVES


{-| -}
string : String -> Value
string =
    JsonValue.Value << JsonString


{-| -}
int : Int -> Value
int =
    JsonValue.Value << JsonNumber << toFloat


{-| -}
float : Float -> Value
float =
    JsonValue.Value << JsonNumber


{-| -}
bool : Bool -> Value
bool =
    JsonValue.Value << JsonBool



-- NULLS


{-| -}
null : Value
null =
    JsonValue.Value JsonNull



-- ARRAYS


{-| -}
list : (a -> Value) -> List a -> Value
list f =
    Array.fromList >> array f


{-| -}
array : (a -> Value) -> Array a -> Value
array f =
    Array.map (f >> JsonValue.unwrap)
        >> JsonArray
        >> JsonValue.Value


{-| -}
set : (a -> Value) -> Set a -> Value
set f =
    Set.toList >> list f



-- OBJECTS


{-| -}
object : List ( String, Value ) -> Value
object =
    List.map (Tuple.mapSecond JsonValue.unwrap)
        >> JsonObject
        >> JsonValue.Value


{-| -}
dict : (k -> String) -> (v -> Value) -> Dict k v -> Value
dict toKey toValue =
    Dict.toList
        >> List.map (Tuple.mapBoth toKey toValue)
        >> object
