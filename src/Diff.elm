module Diff exposing
    ( compareStates
    , findChangedPaths
    , Change(..)
    , DiffResult
    , pathToString
    , DiffOperation(..)
    , diffOperationDecoder
    , applyPatch
    )

{-| State comparison and change detection for the TeaForge Debugger.

This module provides algorithms to compare two JSON state trees and identify
which paths have changed between them. This is used for the diff view to
highlight modified values and optionally filter to show only changes.

It also provides JSON Patch (RFC 6902) application support for the v2 log
format, which stores model changes as a list of patch operations rather than
full state snapshots.

## Comparison

    compareStates : D.Value -> D.Value -> List TreePath
    findChangedPaths : D.Value -> D.Value -> DiffResult

## Patch Application

    applyPatch : D.Value -> List DiffOperation -> D.Value
    diffOperationDecoder : D.Decoder DiffOperation

## Types

    Change : The type of change at a path
    DiffResult : Complete diff result with all changes
    DiffOperation : A single JSON Patch operation

## Utilities

    pathToString : TreePath -> String

-}

import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Set exposing (Set)
import Types exposing (TreePath)


{-| Represents the type of change detected at a path.

  - `Added`: Value exists in "after" but not in "before"
  - `Removed`: Value exists in "before" but not in "after"
  - `Modified`: Value exists in both but has changed
  - `TypeChanged`: Value type has changed (e.g., string to object)

-}
type Change
    = Added
    | Removed
    | Modified
    | TypeChanged


{-| Result of comparing two states.

Contains a dictionary mapping changed paths to their change types,
plus summary counts for each type of change.

-}
type alias DiffResult =
    { changes : Dict String Change
    , changedPaths : List TreePath
    , addedCount : Int
    , removedCount : Int
    , modifiedCount : Int
    }


{-| Compare two state values and return a list of paths that have changed.

This is the main comparison function. It recursively traverses both JSON trees
and identifies all paths where values differ.

    compareStates stateBefore stateAfter
    -- Returns: [ ["user", "name"], ["items", "0", "count"] ]

-}
compareStates : D.Value -> D.Value -> List TreePath
compareStates before after =
    .changedPaths (findChangedPaths before after)


{-| Find all changed paths with detailed change information.

Returns a complete DiffResult with change types and counts.
Use this when you need more detail than just the path list.

    findChangedPaths stateBefore stateAfter
    -- Returns: { changes = ..., changedPaths = [...], addedCount = 1, ... }

-}
findChangedPaths : D.Value -> D.Value -> DiffResult
findChangedPaths before after =
    let
        changes =
            compareValues [] before after

        changedPaths =
            Dict.keys changes
                |> List.map stringToPath
    in
    { changes = changes
    , changedPaths = changedPaths
    , addedCount = countChangeType Added changes
    , removedCount = countChangeType Removed changes
    , modifiedCount = countChangeType Modified changes + countChangeType TypeChanged changes
    }


{-| Convert a TreePath to a string representation.

Joins path segments with dots for display purposes.

    pathToString [ "user", "profile", "name" ]
    -- Returns: "user.profile.name"

-}
pathToString : TreePath -> String
pathToString path =
    String.join "." path



-- INTERNAL COMPARISON FUNCTIONS


{-| Compare two JSON values at a given path, returning all changes found.
-}
compareValues : TreePath -> D.Value -> D.Value -> Dict String Change
compareValues currentPath before after =
    let
        pathKey =
            pathToKey currentPath

        beforeType =
            getValueType before

        afterType =
            getValueType after
    in
    if beforeType /= afterType then
        -- Type changed - record as a change at this path
        Dict.singleton pathKey TypeChanged

    else
        -- Same type - compare based on the type
        case beforeType of
            "object" ->
                compareObjects currentPath before after

            "array" ->
                compareArrays currentPath before after

            "null" ->
                -- Both null - no change
                Dict.empty

            _ ->
                -- Primitive value (string, number, bool)
                if valuesEqual before after then
                    Dict.empty

                else
                    Dict.singleton pathKey Modified


{-| Compare two JSON objects, returning all changed paths.
-}
compareObjects : TreePath -> D.Value -> D.Value -> Dict String Change
compareObjects currentPath before after =
    let
        beforeKeys =
            getObjectKeys before

        afterKeys =
            getObjectKeys after

        allKeys =
            Set.union (Set.fromList beforeKeys) (Set.fromList afterKeys)
                |> Set.toList
    in
    List.foldl
        (\key accDict ->
            let
                childPath =
                    currentPath ++ [ key ]

                beforeValue =
                    getObjectField key before

                afterValue =
                    getObjectField key after

                childChanges =
                    case ( beforeValue, afterValue ) of
                        ( Just bv, Just av ) ->
                            -- Key exists in both - compare values
                            compareValues childPath bv av

                        ( Just bv, Nothing ) ->
                            -- Key removed
                            markPathRemoved childPath bv

                        ( Nothing, Just av ) ->
                            -- Key added
                            markPathAdded childPath av

                        ( Nothing, Nothing ) ->
                            -- Shouldn't happen, but handle gracefully
                            Dict.empty
            in
            Dict.union childChanges accDict
        )
        Dict.empty
        allKeys


{-| Compare two JSON arrays, returning all changed paths.
-}
compareArrays : TreePath -> D.Value -> D.Value -> Dict String Change
compareArrays currentPath before after =
    let
        beforeItems =
            getArrayItems before

        afterItems =
            getArrayItems after

        maxLength =
            max (List.length beforeItems) (List.length afterItems)

        indices =
            List.range 0 (maxLength - 1)
    in
    List.foldl
        (\idx accDict ->
            let
                childPath =
                    currentPath ++ [ String.fromInt idx ]

                beforeItem =
                    List.head (List.drop idx beforeItems)

                afterItem =
                    List.head (List.drop idx afterItems)

                childChanges =
                    case ( beforeItem, afterItem ) of
                        ( Just bv, Just av ) ->
                            -- Element exists in both - compare values
                            compareValues childPath bv av

                        ( Just _, Nothing ) ->
                            -- Element removed
                            Dict.singleton (pathToKey childPath) Removed

                        ( Nothing, Just _ ) ->
                            -- Element added
                            Dict.singleton (pathToKey childPath) Added

                        ( Nothing, Nothing ) ->
                            -- Shouldn't happen, but handle gracefully
                            Dict.empty
            in
            Dict.union childChanges accDict
        )
        Dict.empty
        indices


{-| Mark a path (and all its children) as removed.
-}
markPathRemoved : TreePath -> D.Value -> Dict String Change
markPathRemoved path value =
    case getValueType value of
        "object" ->
            let
                keys =
                    getObjectKeys value

                childChanges =
                    List.foldl
                        (\key acc ->
                            case getObjectField key value of
                                Just childValue ->
                                    Dict.union (markPathRemoved (path ++ [ key ]) childValue) acc

                                Nothing ->
                                    acc
                        )
                        Dict.empty
                        keys
            in
            Dict.insert (pathToKey path) Removed childChanges

        "array" ->
            let
                items =
                    getArrayItems value

                childChanges =
                    List.indexedMap
                        (\idx item -> markPathRemoved (path ++ [ String.fromInt idx ]) item)
                        items
                        |> List.foldl Dict.union Dict.empty
            in
            Dict.insert (pathToKey path) Removed childChanges

        _ ->
            Dict.singleton (pathToKey path) Removed


{-| Mark a path (and all its children) as added.
-}
markPathAdded : TreePath -> D.Value -> Dict String Change
markPathAdded path value =
    case getValueType value of
        "object" ->
            let
                keys =
                    getObjectKeys value

                childChanges =
                    List.foldl
                        (\key acc ->
                            case getObjectField key value of
                                Just childValue ->
                                    Dict.union (markPathAdded (path ++ [ key ]) childValue) acc

                                Nothing ->
                                    acc
                        )
                        Dict.empty
                        keys
            in
            Dict.insert (pathToKey path) Added childChanges

        "array" ->
            let
                items =
                    getArrayItems value

                childChanges =
                    List.indexedMap
                        (\idx item -> markPathAdded (path ++ [ String.fromInt idx ]) item)
                        items
                        |> List.foldl Dict.union Dict.empty
            in
            Dict.insert (pathToKey path) Added childChanges

        _ ->
            Dict.singleton (pathToKey path) Added



-- VALUE TYPE HELPERS


{-| Get the JSON type of a value as a string.
-}
getValueType : D.Value -> String
getValueType value =
    -- Try each type decoder to determine the value type
    if isNull value then
        "null"

    else if isObject value then
        "object"

    else if isArray value then
        "array"

    else if isString value then
        "string"

    else if isNumber value then
        "number"

    else if isBool value then
        "bool"

    else
        "unknown"


{-| Check if a value is null.
-}
isNull : D.Value -> Bool
isNull value =
    case D.decodeValue (D.null ()) value of
        Ok _ ->
            True

        Err _ ->
            False


{-| Check if a value is an object.
-}
isObject : D.Value -> Bool
isObject value =
    case D.decodeValue (D.keyValuePairs D.value) value of
        Ok _ ->
            True

        Err _ ->
            False


{-| Check if a value is an array.
-}
isArray : D.Value -> Bool
isArray value =
    case D.decodeValue (D.list D.value) value of
        Ok _ ->
            True

        Err _ ->
            False


{-| Check if a value is a string.
-}
isString : D.Value -> Bool
isString value =
    case D.decodeValue D.string value of
        Ok _ ->
            True

        Err _ ->
            False


{-| Check if a value is a number.
-}
isNumber : D.Value -> Bool
isNumber value =
    case D.decodeValue D.float value of
        Ok _ ->
            True

        Err _ ->
            False


{-| Check if a value is a boolean.
-}
isBool : D.Value -> Bool
isBool value =
    case D.decodeValue D.bool value of
        Ok _ ->
            True

        Err _ ->
            False



-- OBJECT HELPERS


{-| Get all keys from a JSON object.
-}
getObjectKeys : D.Value -> List String
getObjectKeys value =
    case D.decodeValue (D.keyValuePairs D.value) value of
        Ok pairs ->
            List.map Tuple.first pairs

        Err _ ->
            []


{-| Get a field from a JSON object.
-}
getObjectField : String -> D.Value -> Maybe D.Value
getObjectField key value =
    case D.decodeValue (D.field key D.value) value of
        Ok v ->
            Just v

        Err _ ->
            Nothing



-- ARRAY HELPERS


{-| Get all items from a JSON array.
-}
getArrayItems : D.Value -> List D.Value
getArrayItems value =
    case D.decodeValue (D.list D.value) value of
        Ok items ->
            items

        Err _ ->
            []



-- PATH HELPERS


{-| Convert a path to a string key for the dictionary.
-}
pathToKey : TreePath -> String
pathToKey path =
    String.join "." path


{-| Convert a string key back to a path.
-}
stringToPath : String -> TreePath
stringToPath key =
    if String.isEmpty key then
        []

    else
        String.split "." key



-- VALUE COMPARISON


{-| Check if two JSON values are equal.

Uses JSON encoding to compare values. This handles all JSON types
including nested objects and arrays.

-}
valuesEqual : D.Value -> D.Value -> Bool
valuesEqual v1 v2 =
    E.encode 0 v1 == E.encode 0 v2



-- COUNTING HELPERS


{-| Count changes of a specific type.
-}
countChangeType : Change -> Dict String Change -> Int
countChangeType changeType changes =
    Dict.values changes
        |> List.filter (\c -> c == changeType)
        |> List.length



-- JSON PATCH APPLICATION


{-| A single JSON Patch operation (RFC 6902).

  - `AddOp path value`: set the value at the given JSON Pointer path
  - `ReplaceOp path value`: replace the value at the given JSON Pointer path
  - `RemoveOp path`: remove the value at the given JSON Pointer path

-}
type DiffOperation
    = AddOp String D.Value
    | ReplaceOp String D.Value
    | RemoveOp String


{-| Decoder for a single JSON Patch operation.

Reads the `op` field to determine the operation type, then decodes `path`
and optionally `value`.

-}
diffOperationDecoder : D.Decoder DiffOperation
diffOperationDecoder =
    D.field "op" D.string
        |> D.andThen
            (\op ->
                case op of
                    "add" ->
                        D.map2 AddOp
                            (D.field "path" D.string)
                            (D.field "value" D.value)

                    "replace" ->
                        D.map2 ReplaceOp
                            (D.field "path" D.string)
                            (D.field "value" D.value)

                    "remove" ->
                        D.map RemoveOp
                            (D.field "path" D.string)

                    other ->
                        D.fail ("Unknown JSON Patch op: " ++ other)
            )


{-| Apply a list of JSON Patch operations to a base value.

Folds over the operations, applying each one in sequence. The result is the
accumulated value after all operations have been applied.

-}
applyPatch : D.Value -> List DiffOperation -> D.Value
applyPatch base ops =
    List.foldl applyOp base ops


{-| Apply a single patch operation to a value.
-}
applyOp : DiffOperation -> D.Value -> D.Value
applyOp op base =
    case op of
        AddOp path value ->
            setAtPath (parsePointer path) value base

        ReplaceOp path value ->
            setAtPath (parsePointer path) value base

        RemoveOp path ->
            removeAtPath (parsePointer path) base


{-| Parse a JSON Pointer (RFC 6901) string into a list of path segments.

Handles `~1` → `/` and `~0` → `~` unescaping. A leading `/` is consumed
as the pointer delimiter.

Per RFC 6901, a valid pointer is either the empty string `""` (root, maps to
`[]`) or a string beginning with `/`. A non-empty string that does not start
with `/` is not a valid RFC 6901 pointer; such inputs are treated as root
(returning `[]`) and will be silently mishandled — callers should ensure
only well-formed pointers are passed.

    parsePointer "/foo/bar" == ["foo", "bar"]
    parsePointer "/a~1b" == ["a/b"]
    parsePointer "" == []

-}
parsePointer : String -> List String
parsePointer pointer =
    case String.uncons pointer of
        Just ( '/', rest ) ->
            if String.isEmpty rest then
                [ "" ]

            else
                String.split "/" rest
                    |> List.map unescapePointerSegment

        _ ->
            -- RFC 6901: only the empty string is a valid non-/-prefixed pointer.
            -- Non-empty strings without a leading '/' are invalid; treat as root.
            []


{-| Unescape a single JSON Pointer segment.

Per RFC 6901: `~1` → `/`, `~0` → `~` (in that order).

-}
unescapePointerSegment : String -> String
unescapePointerSegment segment =
    segment
        |> String.replace "~1" "/"
        |> String.replace "~0" "~"


{-| Recursively navigate into a JSON value and set a new value at the leaf path.

For objects, reconstructs the object with the updated field.
For arrays, reconstructs the array with the updated element.
At the root (empty path), returns the new value directly.

-}
setAtPath : List String -> D.Value -> D.Value -> D.Value
setAtPath segments newValue current =
    case segments of
        [] ->
            newValue

        [ key ] ->
            case D.decodeValue (D.keyValuePairs D.value) current of
                Ok pairs ->
                    -- Object: update or insert the key
                    let
                        exists =
                            List.any (\( k, _ ) -> k == key) pairs

                        updatedPairs =
                            if exists then
                                List.map
                                    (\( k, v ) ->
                                        if k == key then
                                            ( k, newValue )

                                        else
                                            ( k, v )
                                    )
                                    pairs

                            else
                                pairs ++ [ ( key, newValue ) ]
                    in
                    E.object updatedPairs

                Err _ ->
                    case D.decodeValue (D.list D.value) current of
                        Ok items ->
                            -- Array: update element at index, or append for RFC 6902 '-' index
                            case String.toInt key of
                                Just idx ->
                                    let
                                        updatedItems =
                                            List.indexedMap
                                                (\i v ->
                                                    if i == idx then
                                                        newValue

                                                    else
                                                        v
                                                )
                                                items
                                    in
                                    E.list identity updatedItems

                                Nothing ->
                                    if key == "-" then
                                        -- RFC 6902: '-' means append to end of array
                                        E.list identity (items ++ [ newValue ])

                                    else
                                        current

                        Err _ ->
                            current

        key :: rest ->
            case D.decodeValue (D.keyValuePairs D.value) current of
                Ok pairs ->
                    let
                        childValue =
                            pairs
                                |> List.filter (\( k, _ ) -> k == key)
                                |> List.head
                                |> Maybe.map Tuple.second
                                |> Maybe.withDefault E.null

                        updatedChild =
                            setAtPath rest newValue childValue

                        exists =
                            List.any (\( k, _ ) -> k == key) pairs

                        updatedPairs =
                            if exists then
                                List.map
                                    (\( k, v ) ->
                                        if k == key then
                                            ( k, updatedChild )

                                        else
                                            ( k, v )
                                    )
                                    pairs

                            else
                                pairs ++ [ ( key, updatedChild ) ]
                    in
                    E.object updatedPairs

                Err _ ->
                    case D.decodeValue (D.list D.value) current of
                        Ok items ->
                            case String.toInt key of
                                Just idx ->
                                    let
                                        childValue =
                                            List.head (List.drop idx items)
                                                |> Maybe.withDefault E.null

                                        updatedChild =
                                            setAtPath rest newValue childValue

                                        updatedItems =
                                            List.indexedMap
                                                (\i v ->
                                                    if i == idx then
                                                        updatedChild

                                                    else
                                                        v
                                                )
                                                items
                                    in
                                    E.list identity updatedItems

                                Nothing ->
                                    if key == "-" then
                                        -- RFC 6902: '-' means append to end of array;
                                        -- navigate into E.null as the new child context
                                        E.list identity (items ++ [ setAtPath rest newValue E.null ])

                                    else
                                        current

                        Err _ ->
                            current


{-| Recursively navigate into a JSON value and remove the value at the leaf path.

For objects, reconstructs the object without the target key.
For arrays, reconstructs the array without the target element.

-}
removeAtPath : List String -> D.Value -> D.Value
removeAtPath segments current =
    case segments of
        [] ->
            -- RFC 6902 does not define removal of the root document; treat as no-op.
            current

        [ key ] ->
            case D.decodeValue (D.keyValuePairs D.value) current of
                Ok pairs ->
                    -- Object: remove the key
                    E.object (List.filter (\( k, _ ) -> k /= key) pairs)

                Err _ ->
                    case D.decodeValue (D.list D.value) current of
                        Ok items ->
                            -- Array: remove element at index
                            case String.toInt key of
                                Just idx ->
                                    E.list identity
                                        (List.indexedMap (\i v -> ( i, v )) items
                                            |> List.filter (\( i, _ ) -> i /= idx)
                                            |> List.map Tuple.second
                                        )

                                Nothing ->
                                    current

                        Err _ ->
                            current

        key :: rest ->
            case D.decodeValue (D.keyValuePairs D.value) current of
                Ok pairs ->
                    let
                        updatedPairs =
                            List.map
                                (\( k, v ) ->
                                    if k == key then
                                        ( k, removeAtPath rest v )

                                    else
                                        ( k, v )
                                )
                                pairs
                    in
                    E.object updatedPairs

                Err _ ->
                    case D.decodeValue (D.list D.value) current of
                        Ok items ->
                            case String.toInt key of
                                Just idx ->
                                    let
                                        updatedItems =
                                            List.indexedMap
                                                (\i v ->
                                                    if i == idx then
                                                        removeAtPath rest v

                                                    else
                                                        v
                                                )
                                                items
                                    in
                                    E.list identity updatedItems

                                Nothing ->
                                    current

                        Err _ ->
                            current
