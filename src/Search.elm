module Search exposing
    ( SearchResult
    , search
    , searchInValue
    , matchesQuery
    , pathToString
    , buildVisiblePaths
    )

{-| Search functionality for the TeaForge Debugger.

This module provides algorithms to search through JSON state trees and find
paths where field names or values match a given query string. The search is
case-insensitive.

## Search

    search : String -> D.Value -> SearchResult
    searchInValue : String -> D.Value -> List TreePath

## Matching

    matchesQuery : String -> String -> Bool

## Utilities

    pathToString : TreePath -> String

-}

import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Set exposing (Set)
import Types exposing (TreePath)


{-| Result of searching a JSON state tree.

Contains:

  - `matches`: List of paths where matches were found
  - `matchCount`: Total number of matches found
  - `pathsWithMatches`: Set of path strings for quick lookup

-}
type alias SearchResult =
    { matches : List TreePath
    , matchCount : Int
    , pathsWithMatches : Set String
    }


{-| Search a JSON value for paths matching the query string.

Searches both field names and values. The search is case-insensitive.
Returns an empty result if the query is empty.

    search "user" jsonValue
    -- Returns: { matches = [["user"], ["currentUser"]], matchCount = 2, ... }

-}
search : String -> D.Value -> SearchResult
search query jsonValue =
    if String.isEmpty (String.trim query) then
        { matches = []
        , matchCount = 0
        , pathsWithMatches = Set.empty
        }

    else
        let
            normalizedQuery =
                String.toLower (String.trim query)

            matches =
                searchInValue normalizedQuery jsonValue

            pathStrings =
                List.map pathToStringInternal matches
                    |> Set.fromList
        in
        { matches = matches
        , matchCount = List.length matches
        , pathsWithMatches = pathStrings
        }


{-| Search a JSON value and return a list of matching paths.

This is the main recursive search function. It traverses the JSON tree and
collects all paths where the field name or value matches the query.

-}
searchInValue : String -> D.Value -> List TreePath
searchInValue query jsonValue =
    searchValueAtPath query [] jsonValue


{-| Recursively search at a specific path in the JSON tree.
-}
searchValueAtPath : String -> TreePath -> D.Value -> List TreePath
searchValueAtPath query currentPath jsonValue =
    let
        -- Check if the current key (last segment of path) matches
        keyMatches =
            case List.head (List.reverse currentPath) of
                Just key ->
                    matchesQuery query key

                Nothing ->
                    False

        -- Get matches from the current value
        valueMatches =
            valueMatchesQuery query jsonValue

        -- This path matches if key or value matches
        thisPathMatches =
            keyMatches || valueMatches

        -- Recurse into children
        childMatches =
            case getValueType jsonValue of
                ObjectType ->
                    searchObject query currentPath jsonValue

                ArrayType ->
                    searchArray query currentPath jsonValue

                _ ->
                    []

        -- Combine current match with child matches
        allMatches =
            if thisPathMatches then
                currentPath :: childMatches

            else
                childMatches
    in
    -- Only include non-empty paths in results
    if List.isEmpty currentPath && thisPathMatches then
        childMatches

    else
        allMatches


{-| Search all fields of a JSON object.
-}
searchObject : String -> TreePath -> D.Value -> List TreePath
searchObject query currentPath jsonValue =
    let
        keys =
            getObjectKeys jsonValue
    in
    List.concatMap
        (\key ->
            let
                childPath =
                    currentPath ++ [ key ]

                childValue =
                    getObjectField key jsonValue
                        |> Maybe.withDefault E.null
            in
            searchValueAtPath query childPath childValue
        )
        keys


{-| Search all elements of a JSON array.
-}
searchArray : String -> TreePath -> D.Value -> List TreePath
searchArray query currentPath jsonValue =
    let
        items =
            getArrayItems jsonValue
    in
    List.indexedMap
        (\idx item ->
            let
                childPath =
                    currentPath ++ [ String.fromInt idx ]
            in
            searchValueAtPath query childPath item
        )
        items
        |> List.concat


{-| Check if a JSON value matches the query string.

For primitive values (strings, numbers, booleans), converts to string and
checks for substring match. For objects and arrays, always returns False
(their children are checked separately).

-}
valueMatchesQuery : String -> D.Value -> Bool
valueMatchesQuery query jsonValue =
    case getValueType jsonValue of
        StringType ->
            case D.decodeValue D.string jsonValue of
                Ok str ->
                    matchesQuery query str

                Err _ ->
                    False

        NumberType ->
            case D.decodeValue D.float jsonValue of
                Ok num ->
                    matchesQuery query (String.fromFloat num)

                Err _ ->
                    False

        BoolType ->
            case D.decodeValue D.bool jsonValue of
                Ok b ->
                    matchesQuery query
                        (if b then
                            "true"

                         else
                            "false"
                        )

                Err _ ->
                    False

        NullType ->
            matchesQuery query "null"

        _ ->
            -- Objects and arrays don't match directly
            False


{-| Check if a string matches the query (case-insensitive substring match).

    matchesQuery "user" "currentUser"
    -- Returns: True

    matchesQuery "name" "firstName"
    -- Returns: True

    matchesQuery "email" "username"
    -- Returns: False

-}
matchesQuery : String -> String -> Bool
matchesQuery query target =
    String.contains (String.toLower query) (String.toLower target)


{-| Convert a TreePath to a string representation.

Joins path segments with dots for display purposes.

    pathToString [ "user", "profile", "name" ]
    -- Returns: "user.profile.name"

-}
pathToString : TreePath -> String
pathToString path =
    String.join "." path


{-| Build a set of all path prefixes that should be visible in filtered mode.

For each matching path, includes the path itself and all its ancestors.
For example, if ["user", "profile", "name"] matches, this returns:
["", "user", "user.profile", "user.profile.name"]

This ensures that when filtering the tree view, parent nodes are shown
so that the matching nodes remain accessible.

-}
buildVisiblePaths : List TreePath -> Set String
buildVisiblePaths matchingPaths =
    List.foldl
        (\path acc ->
            -- Add all prefixes of this path
            List.foldl
                (\idx innerAcc ->
                    let
                        prefix =
                            List.take (idx + 1) path
                                |> String.join "."
                    in
                    Set.insert prefix innerAcc
                )
                (Set.insert "" acc)
                (List.range 0 (List.length path - 1))
        )
        Set.empty
        matchingPaths



-- INTERNAL HELPERS


{-| Internal version of pathToString for use in Set operations.
-}
pathToStringInternal : TreePath -> String
pathToStringInternal =
    pathToString



-- JSON VALUE TYPE HELPERS


type ValueType
    = ObjectType
    | ArrayType
    | StringType
    | NumberType
    | BoolType
    | NullType
    | UnknownType


{-| Determine the type of a JSON value.
-}
getValueType : D.Value -> ValueType
getValueType value =
    if isNull value then
        NullType

    else if isObject value then
        ObjectType

    else if isArray value then
        ArrayType

    else if isString value then
        StringType

    else if isNumber value then
        NumberType

    else if isBool value then
        BoolType

    else
        UnknownType


isNull : D.Value -> Bool
isNull value =
    case D.decodeValue (D.null ()) value of
        Ok _ ->
            True

        Err _ ->
            False


isObject : D.Value -> Bool
isObject value =
    case D.decodeValue (D.keyValuePairs D.value) value of
        Ok _ ->
            True

        Err _ ->
            False


isArray : D.Value -> Bool
isArray value =
    case D.decodeValue (D.list D.value) value of
        Ok _ ->
            True

        Err _ ->
            False


isString : D.Value -> Bool
isString value =
    case D.decodeValue D.string value of
        Ok _ ->
            True

        Err _ ->
            False


isNumber : D.Value -> Bool
isNumber value =
    case D.decodeValue D.float value of
        Ok _ ->
            True

        Err _ ->
            False


isBool : D.Value -> Bool
isBool value =
    case D.decodeValue D.bool value of
        Ok _ ->
            True

        Err _ ->
            False



-- OBJECT HELPERS


getObjectKeys : D.Value -> List String
getObjectKeys value =
    case D.decodeValue (D.keyValuePairs D.value) value of
        Ok pairs ->
            List.map Tuple.first pairs

        Err _ ->
            []


getObjectField : String -> D.Value -> Maybe D.Value
getObjectField key value =
    case D.decodeValue (D.field key D.value) value of
        Ok v ->
            Just v

        Err _ ->
            Nothing



-- ARRAY HELPERS


getArrayItems : D.Value -> List D.Value
getArrayItems value =
    case D.decodeValue (D.list D.value) value of
        Ok items ->
            items

        Err _ ->
            []
