module TreeView exposing
    ( State
    , Config
    , DiffConfig
    , Change(..)
    , Msg
    , init
    , update
    , view
    , viewDiff
    , viewEmpty
    , parseValue
    , expandAll
    , collapseAll
    )

{-| Tree view component for displaying JSON state in the TeaForge Debugger.

This module wraps the elm-json-tree-view library to provide a collapsible/expandable
tree view for model state visualization. It manages expand/collapse state and
provides convenience functions for bulk operations.

Additionally, it provides a diff view mode that highlights changed values and
supports filtering to show only changed paths.

@docs State, Config, DiffConfig, Change, Msg
@docs init, update, view, viewDiff, viewEmpty
@docs parseValue, expandAll, collapseAll

-}

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import JsonTree
import Set exposing (Set)


{-| The internal state for the tree view, managing expand/collapse state.

This wraps JsonTree.State and tracks which nodes are expanded or collapsed.
Store this in your model and update it when tree messages are received.

-}
type alias State =
    { treeState : JsonTree.State
    , root : Maybe JsonTree.Node
    }


{-| Configuration for rendering the tree view.

  - `onSelect`: Optional callback when a node is selected (for future search highlighting)
  - `toMsg`: Function to wrap tree messages into your application's Msg type

-}
type alias Config msg =
    { onSelect : Maybe (JsonTree.KeyPath -> msg)
    , toMsg : Msg -> msg
    }


{-| Change type for diff highlighting.
-}
type Change
    = Added
    | Removed
    | Modified
    | TypeChanged


{-| Configuration for rendering the diff view.

  - `changedPaths`: List of paths that have changed
  - `changes`: Dictionary mapping path strings to change types
  - `changesOnly`: Whether to filter and show only changed paths
  - `onToggleExpand`: Callback when a node is expanded/collapsed

-}
type alias DiffConfig msg =
    { changedPaths : List (List String)
    , changes : Dict String Change
    , changesOnly : Bool
    , onToggleExpand : List String -> msg
    }


{-| Messages for tree view interactions.

These should be handled by calling the `update` function.

-}
type Msg
    = TreeMsg JsonTree.State


{-| Initialize the tree view state.

Call this when setting up your model. The tree starts with no data loaded.

-}
init : State
init =
    { treeState = JsonTree.defaultState
    , root = Nothing
    }


{-| Update the tree view state in response to a message.

This handles expand/collapse interactions from the user.

    update : Msg -> Model -> Model
    update msg model =
        case msg of
            TreeViewMsg treeMsg ->
                { model | treeViewState = TreeView.update treeMsg model.treeViewState }

-}
update : Msg -> State -> State
update msg state =
    case msg of
        TreeMsg newTreeState ->
            { state | treeState = newTreeState }


{-| Parse a JSON value into a tree node structure.

Use this when you receive new JSON data to display. The result is stored
in the State and rendered by the view function.

    case TreeView.parseValue jsonValue of
        Ok newState ->
            { model | treeViewState = newState }

        Err error ->
            -- Handle parse error
            model

-}
parseValue : D.Value -> State -> Result String State
parseValue jsonValue state =
    case JsonTree.parseValue jsonValue of
        Ok node ->
            Ok { state | root = Just node }

        Err error ->
            Err error


{-| Expand all nodes in the tree.

Useful for showing the complete state at once.

-}
expandAll : State -> State
expandAll state =
    case state.root of
        Just root ->
            { state | treeState = JsonTree.expandAll root state.treeState }

        Nothing ->
            state


{-| Collapse all nodes in the tree.

Useful for condensing a large state tree.

-}
collapseAll : State -> State
collapseAll state =
    { state | treeState = JsonTree.defaultState }


{-| Render the tree view.

Displays the JSON tree with collapsible/expandable nodes. If no data is loaded,
shows the empty state placeholder.

-}
view : Config msg -> State -> Html msg
view config state =
    case state.root of
        Just root ->
            div [ class "tree-view font-mono text-sm" ]
                [ JsonTree.view root
                    { onSelect = config.onSelect
                    , toMsg = \newState -> config.toMsg (TreeMsg newState)
                    }
                    state.treeState
                ]

        Nothing ->
            viewEmpty


{-| Render the empty state when no data is loaded.

Shows a centered placeholder message.

-}
viewEmpty : Html msg
viewEmpty =
    div [ class "flex items-center justify-center h-full p-4" ]
        [ div [ class "text-center text-base-content/60" ]
            [ p [ class "text-sm" ] [ text "No state data" ]
            , p [ class "text-xs mt-2" ] [ text "Select a message to view its state" ]
            ]
        ]



-- DIFF VIEW


{-| Internal state for diff view expand/collapse tracking.
-}
type alias DiffState =
    { expandedPaths : Set String
    }


{-| Render the tree view with diff highlighting.

This renders a custom tree that highlights changed values based on the
provided changedPaths. When `changesOnly` is true, only paths that have
changed (and their parents) are shown.

-}
viewDiff : DiffConfig msg -> D.Value -> Set String -> Html msg
viewDiff config jsonValue expandedPaths =
    let
        -- Build a set of path prefixes that should be visible
        visiblePaths =
            if config.changesOnly then
                buildVisiblePaths config.changedPaths

            else
                -- Show all paths when not filtering
                Set.empty
    in
    div [ class "tree-view font-mono text-sm" ]
        [ viewDiffNode config expandedPaths visiblePaths [] jsonValue
        ]


{-| Build a set of all path prefixes that should be visible.

For each changed path, includes the path itself and all its ancestors.
For example, if ["user", "profile", "name"] changed, this returns:
["", "user", "user.profile", "user.profile.name"]

-}
buildVisiblePaths : List (List String) -> Set String
buildVisiblePaths changedPaths =
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
        changedPaths


{-| Render a single node in the diff tree.
-}
viewDiffNode : DiffConfig msg -> Set String -> Set String -> List String -> D.Value -> Html msg
viewDiffNode config expandedPaths visiblePaths currentPath jsonValue =
    let
        pathKey =
            String.join "." currentPath

        -- Check if this path or any child is changed
        hasChanges =
            pathHasChanges pathKey config.changedPaths

        -- Get the change type for this specific path
        changeType =
            Dict.get pathKey config.changes

        -- Should this node be visible?
        isVisible =
            if config.changesOnly then
                Set.member pathKey visiblePaths || pathKey == ""

            else
                True

        -- Highlight class based on change type
        highlightClass =
            case changeType of
                Just Added ->
                    " diff-added"

                Just Removed ->
                    " diff-removed"

                Just Modified ->
                    " diff-changed"

                Just TypeChanged ->
                    " diff-changed"

                Nothing ->
                    ""
    in
    if not isVisible then
        text ""

    else
        case getValueType jsonValue of
            ObjectType ->
                viewDiffObject config expandedPaths visiblePaths currentPath jsonValue highlightClass

            ArrayType ->
                viewDiffArray config expandedPaths visiblePaths currentPath jsonValue highlightClass

            _ ->
                viewDiffPrimitive currentPath jsonValue highlightClass


{-| Check if a path or any of its descendants have changes.
-}
pathHasChanges : String -> List (List String) -> Bool
pathHasChanges pathKey changedPaths =
    List.any
        (\changedPath ->
            let
                changedKey =
                    String.join "." changedPath
            in
            changedKey == pathKey || String.startsWith (pathKey ++ ".") changedKey
        )
        changedPaths


{-| Render an object node with expand/collapse capability.
-}
viewDiffObject : DiffConfig msg -> Set String -> Set String -> List String -> D.Value -> String -> Html msg
viewDiffObject config expandedPaths visiblePaths currentPath jsonValue highlightClass =
    let
        pathKey =
            String.join "." currentPath

        isExpanded =
            Set.member pathKey expandedPaths || pathKey == ""

        keys =
            getObjectKeys jsonValue

        -- Filter keys to only show those with changes when changesOnly is enabled
        visibleKeys =
            if config.changesOnly then
                List.filter
                    (\key ->
                        let
                            childPath =
                                currentPath ++ [ key ]

                            childPathKey =
                                String.join "." childPath
                        in
                        Set.member childPathKey visiblePaths
                    )
                    keys

            else
                keys

        keyCount =
            List.length visibleKeys

        label =
            if List.isEmpty currentPath then
                "root"

            else
                Maybe.withDefault "?" (List.head (List.reverse currentPath))
    in
    div [ class ("tree-node pl-2" ++ highlightClass) ]
        [ div
            [ class "flex items-center gap-1 cursor-pointer hover:bg-base-200 rounded px-1 py-0.5"
            , onClick (config.onToggleExpand currentPath)
            ]
            [ span [ class "text-base-content/40 w-4" ]
                [ text
                    (if isExpanded then
                        "▼"

                     else
                        "▶"
                    )
                ]
            , span [ class "text-primary" ] [ text label ]
            , span [ class "text-base-content/40" ]
                [ text (" {" ++ String.fromInt keyCount ++ "}") ]
            ]
        , if isExpanded then
            div [ class "ml-4 border-l border-base-300 pl-2" ]
                (List.map
                    (\key ->
                        let
                            childPath =
                                currentPath ++ [ key ]

                            childValue =
                                getObjectField key jsonValue
                                    |> Maybe.withDefault E.null
                        in
                        viewDiffKeyValue config expandedPaths visiblePaths childPath key childValue
                    )
                    visibleKeys
                )

          else
            text ""
        ]


{-| Render an array node with expand/collapse capability.
-}
viewDiffArray : DiffConfig msg -> Set String -> Set String -> List String -> D.Value -> String -> Html msg
viewDiffArray config expandedPaths visiblePaths currentPath jsonValue highlightClass =
    let
        pathKey =
            String.join "." currentPath

        isExpanded =
            Set.member pathKey expandedPaths || pathKey == ""

        items =
            getArrayItems jsonValue

        -- Filter items to only show those with changes when changesOnly is enabled
        indexedItems =
            List.indexedMap Tuple.pair items

        visibleItems =
            if config.changesOnly then
                List.filter
                    (\( idx, _ ) ->
                        let
                            childPath =
                                currentPath ++ [ String.fromInt idx ]

                            childPathKey =
                                String.join "." childPath
                        in
                        Set.member childPathKey visiblePaths
                    )
                    indexedItems

            else
                indexedItems

        itemCount =
            List.length visibleItems

        label =
            if List.isEmpty currentPath then
                "root"

            else
                Maybe.withDefault "?" (List.head (List.reverse currentPath))
    in
    div [ class ("tree-node pl-2" ++ highlightClass) ]
        [ div
            [ class "flex items-center gap-1 cursor-pointer hover:bg-base-200 rounded px-1 py-0.5"
            , onClick (config.onToggleExpand currentPath)
            ]
            [ span [ class "text-base-content/40 w-4" ]
                [ text
                    (if isExpanded then
                        "▼"

                     else
                        "▶"
                    )
                ]
            , span [ class "text-primary" ] [ text label ]
            , span [ class "text-base-content/40" ]
                [ text (" [" ++ String.fromInt itemCount ++ "]") ]
            ]
        , if isExpanded then
            div [ class "ml-4 border-l border-base-300 pl-2" ]
                (List.map
                    (\( idx, item ) ->
                        let
                            childPath =
                                currentPath ++ [ String.fromInt idx ]
                        in
                        viewDiffIndexValue config expandedPaths visiblePaths childPath idx item
                    )
                    visibleItems
                )

          else
            text ""
        ]


{-| Render a key-value pair in an object.
-}
viewDiffKeyValue : DiffConfig msg -> Set String -> Set String -> List String -> String -> D.Value -> Html msg
viewDiffKeyValue config expandedPaths visiblePaths currentPath key childValue =
    let
        pathKey =
            String.join "." currentPath

        changeType =
            Dict.get pathKey config.changes

        highlightClass =
            case changeType of
                Just Added ->
                    " diff-added"

                Just Removed ->
                    " diff-removed"

                Just Modified ->
                    " diff-changed"

                Just TypeChanged ->
                    " diff-changed"

                Nothing ->
                    ""
    in
    case getValueType childValue of
        ObjectType ->
            viewDiffObject config expandedPaths visiblePaths currentPath childValue highlightClass

        ArrayType ->
            viewDiffArray config expandedPaths visiblePaths currentPath childValue highlightClass

        _ ->
            div [ class ("flex items-center gap-1 py-0.5 px-1 rounded" ++ highlightClass) ]
                [ span [ class "text-secondary" ] [ text (key ++ ":") ]
                , viewPrimitiveValue childValue
                ]


{-| Render an indexed value in an array.
-}
viewDiffIndexValue : DiffConfig msg -> Set String -> Set String -> List String -> Int -> D.Value -> Html msg
viewDiffIndexValue config expandedPaths visiblePaths currentPath idx childValue =
    let
        pathKey =
            String.join "." currentPath

        changeType =
            Dict.get pathKey config.changes

        highlightClass =
            case changeType of
                Just Added ->
                    " diff-added"

                Just Removed ->
                    " diff-removed"

                Just Modified ->
                    " diff-changed"

                Just TypeChanged ->
                    " diff-changed"

                Nothing ->
                    ""
    in
    case getValueType childValue of
        ObjectType ->
            viewDiffObject config expandedPaths visiblePaths currentPath childValue highlightClass

        ArrayType ->
            viewDiffArray config expandedPaths visiblePaths currentPath childValue highlightClass

        _ ->
            div [ class ("flex items-center gap-1 py-0.5 px-1 rounded" ++ highlightClass) ]
                [ span [ class "text-base-content/40" ] [ text (String.fromInt idx ++ ":") ]
                , viewPrimitiveValue childValue
                ]


{-| Render a primitive value (string, number, bool, null) without a key.
-}
viewDiffPrimitive : List String -> D.Value -> String -> Html msg
viewDiffPrimitive currentPath jsonValue highlightClass =
    let
        label =
            if List.isEmpty currentPath then
                ""

            else
                Maybe.withDefault "" (List.head (List.reverse currentPath)) ++ ": "
    in
    div [ class ("flex items-center gap-1 py-0.5 px-1 rounded" ++ highlightClass) ]
        [ if not (String.isEmpty label) then
            span [ class "text-secondary" ] [ text label ]

          else
            text ""
        , viewPrimitiveValue jsonValue
        ]


{-| Render a primitive JSON value with appropriate styling.
-}
viewPrimitiveValue : D.Value -> Html msg
viewPrimitiveValue jsonValue =
    case D.decodeValue D.string jsonValue of
        Ok str ->
            span [ class "tree-value-string" ] [ text ("\"" ++ truncateString 100 str ++ "\"") ]

        Err _ ->
            case D.decodeValue D.float jsonValue of
                Ok num ->
                    span [ class "tree-value-number" ] [ text (String.fromFloat num) ]

                Err _ ->
                    case D.decodeValue D.bool jsonValue of
                        Ok b ->
                            span [ class "tree-value-boolean" ]
                                [ text
                                    (if b then
                                        "true"

                                     else
                                        "false"
                                    )
                                ]

                        Err _ ->
                            case D.decodeValue (D.null ()) jsonValue of
                                Ok _ ->
                                    span [ class "tree-value-null" ] [ text "null" ]

                                Err _ ->
                                    span [] [ text (E.encode 0 jsonValue) ]


{-| Truncate a string if it's too long.
-}
truncateString : Int -> String -> String
truncateString maxLen str =
    if String.length str > maxLen then
        String.left maxLen str ++ "..."

    else
        str



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


getArrayItems : D.Value -> List D.Value
getArrayItems value =
    case D.decodeValue (D.list D.value) value of
        Ok items ->
            items

        Err _ ->
            []
