module TreeView exposing
    ( State
    , Config
    , DiffConfig
    , FilterConfig
    , UnifiedConfig
    , Change(..)
    , Msg
    , init
    , update
    , view
    , viewUnified
    , viewFiltered
    , viewDiff
    , viewDiffBefore
    , viewEmpty
    , parseValue
    , expandAll
    , collapseAll
    , pathToElementId
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
  - `searchMatches`: Set of paths that match the search query (for highlighting)
  - `currentMatchPath`: The path of the current search match (for special highlighting)

-}
type alias DiffConfig msg =
    { changedPaths : List (List String)
    , changes : Dict String Change
    , changesOnly : Bool
    , onToggleExpand : List String -> msg
    , searchMatches : Set String
    , currentMatchPath : Maybe String
    }


{-| Configuration for rendering the filtered tree view.

  - `visiblePaths`: Set of path strings that should be visible
  - `matchingPaths`: Set of paths that directly match the search (for highlighting)
  - `currentMatchPath`: The path of the current search match (for special highlighting)
  - `onToggleExpand`: Callback when a node is expanded/collapsed

-}
type alias FilterConfig msg =
    { visiblePaths : Set String
    , matchingPaths : Set String
    , currentMatchPath : Maybe String
    , onToggleExpand : List String -> msg
    }


{-| Configuration for rendering the unified tree view.

This uses the same styling as the diff view but without change highlighting.

  - `onToggleExpand`: Callback when a node is expanded/collapsed
  - `searchMatches`: Set of paths that match the search query (for highlighting)
  - `currentMatchPath`: The path of the current search match (for special highlighting)

-}
type alias UnifiedConfig msg =
    { onToggleExpand : List String -> msg
    , searchMatches : Set String
    , currentMatchPath : Maybe String
    }


{-| Messages for tree view interactions.

These should be handled by calling the `update` function.

-}
type Msg
    = TreeMsg JsonTree.State


{-| Compute the search highlight class for a given path.
-}
searchHighlightClass : Set String -> Maybe String -> String -> String
searchHighlightClass searchMatches currentMatchPath pathKey =
    if Just pathKey == currentMatchPath then
        " search-match-current"

    else if Set.member pathKey searchMatches then
        " search-match"

    else
        ""


{-| Convert a path key to a valid HTML element ID.

Replaces dots with dashes and adds a prefix to ensure validity.

-}
pathToElementId : String -> String
pathToElementId pathKey =
    "tree-node-" ++ String.replace "." "-" pathKey


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
            Err (D.errorToString error)


{-| Expand all nodes in the tree.

Useful for showing the complete state at once.

-}
expandAll : State -> State
expandAll state =
    case state.root of
        Just _ ->
            { state | treeState = JsonTree.expandAll state.treeState }

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
                    { colors = JsonTree.defaultColors
                    , onSelect = config.onSelect
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



-- UNIFIED VIEW (same styling as diff view, but without diff highlighting)


{-| Render the tree view using the unified styling (same as diff view).

This provides consistent styling across all tree views by using the same
custom tree rendering as viewDiff, but without any change highlighting.
Skips the redundant root object wrapper since values are always objects.

-}
viewUnified : UnifiedConfig msg -> D.Value -> Set String -> Html msg
viewUnified config jsonValue expandedPaths =
    div [ class "tree-view font-mono text-sm" ]
        [ viewUnifiedRootChildren config expandedPaths jsonValue
        ]


{-| Render the children of the root object directly, skipping the root wrapper.
-}
viewUnifiedRootChildren : UnifiedConfig msg -> Set String -> D.Value -> Html msg
viewUnifiedRootChildren config expandedPaths jsonValue =
    let
        keys =
            getObjectKeysWithoutType jsonValue
    in
    div [ class "pl-2" ]
        (List.map
            (\key ->
                let
                    childPath =
                        [ key ]

                    childValue =
                        getObjectField key jsonValue
                            |> Maybe.withDefault E.null
                in
                viewUnifiedKeyValue config expandedPaths childPath key childValue
            )
            keys
        )


{-| Render a single node in the unified tree.
-}
viewUnifiedNode : UnifiedConfig msg -> Set String -> List String -> D.Value -> Html msg
viewUnifiedNode config expandedPaths currentPath jsonValue =
    case getValueType jsonValue of
        ObjectType ->
            viewUnifiedObject config expandedPaths currentPath jsonValue

        ArrayType ->
            viewUnifiedArray config expandedPaths currentPath jsonValue

        _ ->
            viewUnifiedPrimitive config currentPath jsonValue


{-| Render an object node in the unified tree with expand/collapse capability.
-}
viewUnifiedObject : UnifiedConfig msg -> Set String -> List String -> D.Value -> Html msg
viewUnifiedObject config expandedPaths currentPath jsonValue =
    let
        pathKey =
            String.join "." currentPath

        isExpanded =
            Set.member pathKey expandedPaths || pathKey == ""

        keys =
            getObjectKeysWithoutType jsonValue

        hasChildren =
            not (List.isEmpty keys)

        typeAnnotation =
            getTypeAnnotation jsonValue

        label =
            if List.isEmpty currentPath then
                "root"

            else
                Maybe.withDefault "?" (List.head (List.reverse currentPath))

        highlightClass =
            searchHighlightClass config.searchMatches config.currentMatchPath pathKey

        elementId =
            pathToElementId pathKey

        labelParts =
            case typeAnnotation of
                Just typeName ->
                    [ span [ class "text-primary" ] [ text label ]
                    , span [ class "text-base-content/40" ] [ text " : " ]
                    , span [ class "text-info" ] [ text typeName ]
                    ]

                Nothing ->
                    [ span [ class "text-primary" ] [ text label ] ]

        arrowAndInteraction =
            if hasChildren then
                ( [ span [ class "text-base-content/40 w-4" ]
                        [ text
                            (if isExpanded then
                                "▼"

                             else
                                "▶"
                            )
                        ]
                  ]
                , [ class ("inline-flex items-center gap-1 cursor-pointer hover:bg-base-200 rounded px-1 py-0.5" ++ highlightClass)
                  , onClick (config.onToggleExpand currentPath)
                  ]
                )

            else
                ( [ span [ class "text-base-content/40 w-4" ] [ text "" ] ]
                , [ class ("inline-flex items-center gap-1 rounded px-1 py-0.5" ++ highlightClass) ]
                )

        ( arrowPart, interactionAttrs ) =
            arrowAndInteraction
    in
    div [ class "tree-node pl-2" ]
        [ div
            ([ id elementId ] ++ interactionAttrs)
            (arrowPart ++ labelParts)
        , if isExpanded && hasChildren then
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
                        viewUnifiedKeyValue config expandedPaths childPath key childValue
                    )
                    keys
                )

          else
            text ""
        ]


{-| Render an array node in the unified tree with expand/collapse capability.
-}
viewUnifiedArray : UnifiedConfig msg -> Set String -> List String -> D.Value -> Html msg
viewUnifiedArray config expandedPaths currentPath jsonValue =
    let
        pathKey =
            String.join "." currentPath

        isExpanded =
            Set.member pathKey expandedPaths || pathKey == ""

        items =
            getArrayItems jsonValue

        itemCount =
            List.length items

        label =
            if List.isEmpty currentPath then
                "root"

            else
                Maybe.withDefault "?" (List.head (List.reverse currentPath))

        highlightClass =
            searchHighlightClass config.searchMatches config.currentMatchPath pathKey

        elementId =
            pathToElementId pathKey
    in
    div [ class "tree-node pl-2" ]
        [ div
            [ id elementId
            , class ("inline-flex items-center gap-1 cursor-pointer hover:bg-base-200 rounded px-1 py-0.5" ++ highlightClass)
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
                (List.indexedMap
                    (\idx item ->
                        let
                            childPath =
                                currentPath ++ [ String.fromInt idx ]
                        in
                        viewUnifiedIndexValue config expandedPaths childPath idx item
                    )
                    items
                )

          else
            text ""
        ]


{-| Render a key-value pair in the unified tree.
-}
viewUnifiedKeyValue : UnifiedConfig msg -> Set String -> List String -> String -> D.Value -> Html msg
viewUnifiedKeyValue config expandedPaths currentPath key childValue =
    let
        pathKey =
            String.join "." currentPath

        highlightClass =
            searchHighlightClass config.searchMatches config.currentMatchPath pathKey

        elementId =
            pathToElementId pathKey
    in
    case getValueType childValue of
        ObjectType ->
            viewUnifiedObject config expandedPaths currentPath childValue

        ArrayType ->
            viewUnifiedArray config expandedPaths currentPath childValue

        _ ->
            div [ class "tree-node pl-2" ]
                [ div [ id elementId, class ("inline-flex items-center gap-1 py-0.5 px-1 rounded" ++ highlightClass) ]
                    [ span [ class "text-base-content/40 w-4" ] [ text "" ]
                    , span [ class "text-secondary" ] [ text (key ++ ":") ]
                    , viewPrimitiveValue childValue
                    ]
                ]


{-| Render an indexed value in the unified array.
-}
viewUnifiedIndexValue : UnifiedConfig msg -> Set String -> List String -> Int -> D.Value -> Html msg
viewUnifiedIndexValue config expandedPaths currentPath idx childValue =
    let
        pathKey =
            String.join "." currentPath

        highlightClass =
            searchHighlightClass config.searchMatches config.currentMatchPath pathKey

        elementId =
            pathToElementId pathKey
    in
    case getValueType childValue of
        ObjectType ->
            viewUnifiedObject config expandedPaths currentPath childValue

        ArrayType ->
            viewUnifiedArray config expandedPaths currentPath childValue

        _ ->
            div [ class "tree-node pl-2" ]
                [ div [ id elementId, class ("inline-flex items-center gap-1 py-0.5 px-1 rounded" ++ highlightClass) ]
                    [ span [ class "text-base-content/40 w-4" ] [ text "" ]
                    , span [ class "text-base-content/40" ] [ text (String.fromInt idx ++ ":") ]
                    , viewPrimitiveValue childValue
                    ]
                ]


{-| Render a primitive value in the unified tree.
-}
viewUnifiedPrimitive : UnifiedConfig msg -> List String -> D.Value -> Html msg
viewUnifiedPrimitive config currentPath jsonValue =
    let
        pathKey =
            String.join "." currentPath

        label =
            if List.isEmpty currentPath then
                ""

            else
                Maybe.withDefault "" (List.head (List.reverse currentPath)) ++ ": "

        highlightClass =
            searchHighlightClass config.searchMatches config.currentMatchPath pathKey

        elementId =
            pathToElementId pathKey
    in
    div [ id elementId, class ("inline-flex items-center gap-1 py-0.5 px-1 rounded" ++ highlightClass) ]
        [ if not (String.isEmpty label) then
            span [ class "text-secondary" ] [ text label ]

          else
            text ""
        , viewPrimitiveValue jsonValue
        ]



-- FILTERED VIEW (for search filter mode)


{-| Render the tree view with search filtering.

When filter mode is active, this shows only paths that match the search query
(and their parent paths to maintain tree structure). Matching nodes are
highlighted with a special style. Skips the redundant root object wrapper.

-}
viewFiltered : FilterConfig msg -> D.Value -> Set String -> Html msg
viewFiltered config jsonValue expandedPaths =
    if Set.isEmpty config.visiblePaths then
        -- No matches, show empty state
        div [ class "tree-view font-mono text-sm" ]
            [ div [ class "flex items-center justify-center h-32 text-base-content/60" ]
                [ div [ class "text-center" ]
                    [ p [ class "text-sm" ] [ text "No matches found" ]
                    , p [ class "text-xs mt-1" ] [ text "Try a different search term" ]
                    ]
                ]
            ]

    else
        div [ class "tree-view font-mono text-sm" ]
            [ viewFilteredRootChildren config expandedPaths jsonValue
            ]


{-| Render the children of the root object directly for filtered view.
-}
viewFilteredRootChildren : FilterConfig msg -> Set String -> D.Value -> Html msg
viewFilteredRootChildren config expandedPaths jsonValue =
    let
        keys =
            getObjectKeysWithoutType jsonValue

        -- Filter keys to only show those that are visible
        visibleKeys =
            List.filter
                (\key ->
                    let
                        childPathKey =
                            key
                    in
                    Set.member childPathKey config.visiblePaths
                )
                keys
    in
    div [ class "pl-2" ]
        (List.map
            (\key ->
                let
                    childPath =
                        [ key ]

                    childValue =
                        getObjectField key jsonValue
                            |> Maybe.withDefault E.null
                in
                viewFilteredKeyValue config expandedPaths childPath key childValue
            )
            visibleKeys
        )


{-| Render a single node in the filtered tree.
-}
viewFilteredNode : FilterConfig msg -> Set String -> List String -> D.Value -> Html msg
viewFilteredNode config expandedPaths currentPath jsonValue =
    let
        pathKey =
            String.join "." currentPath

        -- Should this node be visible?
        isVisible =
            Set.member pathKey config.visiblePaths || pathKey == ""

        -- Highlight class for matching nodes (uses current match for special highlighting)
        highlightClass =
            searchHighlightClass config.matchingPaths config.currentMatchPath pathKey
    in
    if not isVisible then
        text ""

    else
        case getValueType jsonValue of
            ObjectType ->
                viewFilteredObject config expandedPaths currentPath jsonValue highlightClass

            ArrayType ->
                viewFilteredArray config expandedPaths currentPath jsonValue highlightClass

            _ ->
                viewFilteredPrimitive currentPath jsonValue highlightClass


{-| Render an object node in the filtered tree with expand/collapse capability.
-}
viewFilteredObject : FilterConfig msg -> Set String -> List String -> D.Value -> String -> Html msg
viewFilteredObject config expandedPaths currentPath jsonValue highlightClass =
    let
        pathKey =
            String.join "." currentPath

        isExpanded =
            Set.member pathKey expandedPaths || pathKey == ""

        keys =
            getObjectKeysWithoutType jsonValue

        -- Filter keys to only show those that are visible
        visibleKeys =
            List.filter
                (\key ->
                    let
                        childPath =
                            currentPath ++ [ key ]

                        childPathKey =
                            String.join "." childPath
                    in
                    Set.member childPathKey config.visiblePaths
                )
                keys

        hasChildren =
            not (List.isEmpty visibleKeys)

        typeAnnotation =
            getTypeAnnotation jsonValue

        label =
            if List.isEmpty currentPath then
                "root"

            else
                Maybe.withDefault "?" (List.head (List.reverse currentPath))

        elementId =
            pathToElementId pathKey

        labelParts =
            case typeAnnotation of
                Just typeName ->
                    [ span [ class "text-primary" ] [ text label ]
                    , span [ class "text-base-content/40" ] [ text " : " ]
                    , span [ class "text-info" ] [ text typeName ]
                    ]

                Nothing ->
                    [ span [ class "text-primary" ] [ text label ] ]

        arrowAndInteraction =
            if hasChildren then
                ( [ span [ class "text-base-content/40 w-4" ]
                        [ text
                            (if isExpanded then
                                "▼"

                             else
                                "▶"
                            )
                        ]
                  ]
                , [ class ("inline-flex items-center gap-1 cursor-pointer hover:bg-base-200 rounded px-1 py-0.5" ++ highlightClass)
                  , onClick (config.onToggleExpand currentPath)
                  ]
                )

            else
                ( [ span [ class "text-base-content/40 w-4" ] [ text "" ] ]
                , [ class ("inline-flex items-center gap-1 rounded px-1 py-0.5" ++ highlightClass) ]
                )

        ( arrowPart, interactionAttrs ) =
            arrowAndInteraction
    in
    div [ class "tree-node pl-2" ]
        [ div
            ([ id elementId ] ++ interactionAttrs)
            (arrowPart ++ labelParts)
        , if isExpanded && hasChildren then
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
                        viewFilteredKeyValue config expandedPaths childPath key childValue
                    )
                    visibleKeys
                )

          else
            text ""
        ]


{-| Render an array node in the filtered tree with expand/collapse capability.
-}
viewFilteredArray : FilterConfig msg -> Set String -> List String -> D.Value -> String -> Html msg
viewFilteredArray config expandedPaths currentPath jsonValue highlightClass =
    let
        pathKey =
            String.join "." currentPath

        isExpanded =
            Set.member pathKey expandedPaths || pathKey == ""

        items =
            getArrayItems jsonValue

        -- Filter items to only show those that are visible
        indexedItems =
            List.indexedMap Tuple.pair items

        visibleItems =
            List.filter
                (\( idx, _ ) ->
                    let
                        childPath =
                            currentPath ++ [ String.fromInt idx ]

                        childPathKey =
                            String.join "." childPath
                    in
                    Set.member childPathKey config.visiblePaths
                )
                indexedItems

        itemCount =
            List.length visibleItems

        label =
            if List.isEmpty currentPath then
                "root"

            else
                Maybe.withDefault "?" (List.head (List.reverse currentPath))

        elementId =
            pathToElementId pathKey
    in
    div [ class "tree-node pl-2" ]
        [ div
            [ id elementId
            , class ("inline-flex items-center gap-1 cursor-pointer hover:bg-base-200 rounded px-1 py-0.5" ++ highlightClass)
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
                        viewFilteredIndexValue config expandedPaths childPath idx item
                    )
                    visibleItems
                )

          else
            text ""
        ]


{-| Render a key-value pair in the filtered tree.
-}
viewFilteredKeyValue : FilterConfig msg -> Set String -> List String -> String -> D.Value -> Html msg
viewFilteredKeyValue config expandedPaths currentPath key childValue =
    let
        pathKey =
            String.join "." currentPath

        highlightClass =
            searchHighlightClass config.matchingPaths config.currentMatchPath pathKey

        elementId =
            pathToElementId pathKey
    in
    case getValueType childValue of
        ObjectType ->
            viewFilteredObject config expandedPaths currentPath childValue highlightClass

        ArrayType ->
            viewFilteredArray config expandedPaths currentPath childValue highlightClass

        _ ->
            div [ class "tree-node pl-2" ]
                [ div [ id elementId, class ("inline-flex items-center gap-1 py-0.5 px-1 rounded" ++ highlightClass) ]
                    [ span [ class "text-base-content/40 w-4" ] [ text "" ]
                    , span [ class "text-secondary" ] [ text (key ++ ":") ]
                    , viewPrimitiveValue childValue
                    ]
                ]


{-| Render an indexed value in the filtered array.
-}
viewFilteredIndexValue : FilterConfig msg -> Set String -> List String -> Int -> D.Value -> Html msg
viewFilteredIndexValue config expandedPaths currentPath idx childValue =
    let
        pathKey =
            String.join "." currentPath

        highlightClass =
            searchHighlightClass config.matchingPaths config.currentMatchPath pathKey

        elementId =
            pathToElementId pathKey
    in
    case getValueType childValue of
        ObjectType ->
            viewFilteredObject config expandedPaths currentPath childValue highlightClass

        ArrayType ->
            viewFilteredArray config expandedPaths currentPath childValue highlightClass

        _ ->
            div [ class "tree-node pl-2" ]
                [ div [ id elementId, class ("inline-flex items-center gap-1 py-0.5 px-1 rounded" ++ highlightClass) ]
                    [ span [ class "text-base-content/40 w-4" ] [ text "" ]
                    , span [ class "text-base-content/40" ] [ text (String.fromInt idx ++ ":") ]
                    , viewPrimitiveValue childValue
                    ]
                ]


{-| Render a primitive value in the filtered tree.
-}
viewFilteredPrimitive : List String -> D.Value -> String -> Html msg
viewFilteredPrimitive currentPath jsonValue highlightClass =
    let
        pathKey =
            String.join "." currentPath

        label =
            if List.isEmpty currentPath then
                ""

            else
                Maybe.withDefault "" (List.head (List.reverse currentPath)) ++ ": "

        elementId =
            pathToElementId pathKey
    in
    div [ id elementId, class ("inline-flex items-center gap-1 py-0.5 px-1 rounded" ++ highlightClass) ]
        [ if not (String.isEmpty label) then
            span [ class "text-secondary" ] [ text label ]

          else
            text ""
        , viewPrimitiveValue jsonValue
        ]



-- DIFF VIEW


{-| Internal state for diff view expand/collapse tracking.
-}
type alias DiffState =
    { expandedPaths : Set String
    }


{-| Configuration for rendering the before diff view.

Similar to DiffConfig but used for the "before" state where we show
removed fields with removed styling.

  - `changedPaths`: List of paths that have changed
  - `changes`: Dictionary mapping path strings to change types
  - `onToggleExpand`: Callback when a node is expanded/collapsed
  - `searchMatches`: Set of paths that match the search query (for highlighting)
  - `currentMatchPath`: The path of the current search match (for special highlighting)

-}
type alias BeforeDiffConfig msg =
    { changedPaths : List (List String)
    , changes : Dict String Change
    , onToggleExpand : List String -> msg
    , searchMatches : Set String
    , currentMatchPath : Maybe String
    }


{-| Render the tree view with diff highlighting for the "before" state.

This renders a custom tree that highlights values that will be removed or
modified. Removed items show with "removed" styling, modified items show
with "changed" styling (indicating the old value is being changed).
Skips the redundant root object wrapper.

-}
viewDiffBefore : BeforeDiffConfig msg -> D.Value -> Set String -> Html msg
viewDiffBefore config jsonValue expandedPaths =
    div [ class "tree-view font-mono text-sm" ]
        [ viewDiffBeforeRootChildren config expandedPaths jsonValue
        ]


{-| Render the children of the root object directly for before diff view.
-}
viewDiffBeforeRootChildren : BeforeDiffConfig msg -> Set String -> D.Value -> Html msg
viewDiffBeforeRootChildren config expandedPaths jsonValue =
    let
        keys =
            getObjectKeysWithoutType jsonValue
    in
    div [ class "pl-2" ]
        (List.map
            (\key ->
                let
                    childPath =
                        [ key ]

                    childValue =
                        getObjectField key jsonValue
                            |> Maybe.withDefault E.null
                in
                viewDiffBeforeKeyValue config expandedPaths childPath key childValue
            )
            keys
        )


{-| Render a single node in the before diff tree.
-}
viewDiffBeforeNode : BeforeDiffConfig msg -> Set String -> List String -> D.Value -> Html msg
viewDiffBeforeNode config expandedPaths currentPath jsonValue =
    let
        pathKey =
            String.join "." currentPath

        -- Get the change type for this specific path
        changeType =
            Dict.get pathKey config.changes

        -- Diff highlight class based on change type for "before" view
        -- Removed items in before = they existed before and are removed
        -- Modified items in before = these are the old values being changed
        -- Added items don't exist in the before state
        diffClass =
            case changeType of
                Just Removed ->
                    " diff-removed"

                Just Modified ->
                    " diff-changed"

                Just TypeChanged ->
                    " diff-changed"

                Just Added ->
                    -- Added items shouldn't appear in before state
                    ""

                Nothing ->
                    ""

        -- Search highlight class
        searchClass =
            searchHighlightClass config.searchMatches config.currentMatchPath pathKey

        -- Combine both classes
        highlightClass =
            diffClass ++ searchClass
    in
    case getValueType jsonValue of
        ObjectType ->
            viewDiffBeforeObject config expandedPaths currentPath jsonValue highlightClass

        ArrayType ->
            viewDiffBeforeArray config expandedPaths currentPath jsonValue highlightClass

        _ ->
            viewDiffBeforePrimitive currentPath jsonValue highlightClass


{-| Render an object node in the before diff view.
-}
viewDiffBeforeObject : BeforeDiffConfig msg -> Set String -> List String -> D.Value -> String -> Html msg
viewDiffBeforeObject config expandedPaths currentPath jsonValue highlightClass =
    let
        pathKey =
            String.join "." currentPath

        isExpanded =
            Set.member pathKey expandedPaths || pathKey == ""

        keys =
            getObjectKeysWithoutType jsonValue

        hasChildren =
            not (List.isEmpty keys)

        typeAnnotation =
            getTypeAnnotation jsonValue

        label =
            if List.isEmpty currentPath then
                "root"

            else
                Maybe.withDefault "?" (List.head (List.reverse currentPath))

        elementId =
            pathToElementId pathKey

        labelParts =
            case typeAnnotation of
                Just typeName ->
                    [ span [ class "text-primary" ] [ text label ]
                    , span [ class "text-base-content/40" ] [ text " : " ]
                    , span [ class "text-info" ] [ text typeName ]
                    ]

                Nothing ->
                    [ span [ class "text-primary" ] [ text label ] ]

        arrowAndInteraction =
            if hasChildren then
                ( [ span [ class "text-base-content/40 w-4" ]
                        [ text
                            (if isExpanded then
                                "▼"

                             else
                                "▶"
                            )
                        ]
                  ]
                , [ class ("inline-flex items-center gap-1 cursor-pointer hover:bg-base-200 rounded px-1 py-0.5" ++ highlightClass)
                  , onClick (config.onToggleExpand currentPath)
                  ]
                )

            else
                ( [ span [ class "text-base-content/40 w-4" ] [ text "" ] ]
                , [ class ("inline-flex items-center gap-1 rounded px-1 py-0.5" ++ highlightClass) ]
                )

        ( arrowPart, interactionAttrs ) =
            arrowAndInteraction
    in
    div [ class "tree-node pl-2" ]
        [ div
            ([ id elementId ] ++ interactionAttrs)
            (arrowPart ++ labelParts)
        , if isExpanded && hasChildren then
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
                        viewDiffBeforeKeyValue config expandedPaths childPath key childValue
                    )
                    keys
                )

          else
            text ""
        ]


{-| Render an array node in the before diff view.
-}
viewDiffBeforeArray : BeforeDiffConfig msg -> Set String -> List String -> D.Value -> String -> Html msg
viewDiffBeforeArray config expandedPaths currentPath jsonValue highlightClass =
    let
        pathKey =
            String.join "." currentPath

        isExpanded =
            Set.member pathKey expandedPaths || pathKey == ""

        items =
            getArrayItems jsonValue

        itemCount =
            List.length items

        label =
            if List.isEmpty currentPath then
                "root"

            else
                Maybe.withDefault "?" (List.head (List.reverse currentPath))

        elementId =
            pathToElementId pathKey
    in
    div [ class "tree-node pl-2" ]
        [ div
            [ id elementId
            , class ("inline-flex items-center gap-1 cursor-pointer hover:bg-base-200 rounded px-1 py-0.5" ++ highlightClass)
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
                (List.indexedMap
                    (\idx item ->
                        let
                            childPath =
                                currentPath ++ [ String.fromInt idx ]
                        in
                        viewDiffBeforeIndexValue config expandedPaths childPath idx item
                    )
                    items
                )

          else
            text ""
        ]


{-| Render a key-value pair in the before diff view.
-}
viewDiffBeforeKeyValue : BeforeDiffConfig msg -> Set String -> List String -> String -> D.Value -> Html msg
viewDiffBeforeKeyValue config expandedPaths currentPath key childValue =
    let
        pathKey =
            String.join "." currentPath

        changeType =
            Dict.get pathKey config.changes

        diffClass =
            case changeType of
                Just Removed ->
                    " diff-removed"

                Just Modified ->
                    " diff-changed"

                Just TypeChanged ->
                    " diff-changed"

                Just Added ->
                    ""

                Nothing ->
                    ""

        searchClass =
            searchHighlightClass config.searchMatches config.currentMatchPath pathKey

        highlightClass =
            diffClass ++ searchClass

        elementId =
            pathToElementId pathKey
    in
    case getValueType childValue of
        ObjectType ->
            viewDiffBeforeObject config expandedPaths currentPath childValue highlightClass

        ArrayType ->
            viewDiffBeforeArray config expandedPaths currentPath childValue highlightClass

        _ ->
            div [ class "tree-node pl-2" ]
                [ div [ id elementId, class ("inline-flex items-center gap-1 py-0.5 px-1 rounded" ++ highlightClass) ]
                    [ span [ class "text-base-content/40 w-4" ] [ text "" ]
                    , span [ class "text-secondary" ] [ text (key ++ ":") ]
                    , viewPrimitiveValue childValue
                    ]
                ]


{-| Render an indexed value in the before diff array.
-}
viewDiffBeforeIndexValue : BeforeDiffConfig msg -> Set String -> List String -> Int -> D.Value -> Html msg
viewDiffBeforeIndexValue config expandedPaths currentPath idx childValue =
    let
        pathKey =
            String.join "." currentPath

        changeType =
            Dict.get pathKey config.changes

        diffClass =
            case changeType of
                Just Removed ->
                    " diff-removed"

                Just Modified ->
                    " diff-changed"

                Just TypeChanged ->
                    " diff-changed"

                Just Added ->
                    ""

                Nothing ->
                    ""

        searchClass =
            searchHighlightClass config.searchMatches config.currentMatchPath pathKey

        highlightClass =
            diffClass ++ searchClass

        elementId =
            pathToElementId pathKey
    in
    case getValueType childValue of
        ObjectType ->
            viewDiffBeforeObject config expandedPaths currentPath childValue highlightClass

        ArrayType ->
            viewDiffBeforeArray config expandedPaths currentPath childValue highlightClass

        _ ->
            div [ class "tree-node pl-2" ]
                [ div [ id elementId, class ("inline-flex items-center gap-1 py-0.5 px-1 rounded" ++ highlightClass) ]
                    [ span [ class "text-base-content/40 w-4" ] [ text "" ]
                    , span [ class "text-base-content/40" ] [ text (String.fromInt idx ++ ":") ]
                    , viewPrimitiveValue childValue
                    ]
                ]


{-| Render a primitive value in the before diff view.
-}
viewDiffBeforePrimitive : List String -> D.Value -> String -> Html msg
viewDiffBeforePrimitive currentPath jsonValue highlightClass =
    let
        pathKey =
            String.join "." currentPath

        label =
            if List.isEmpty currentPath then
                ""

            else
                Maybe.withDefault "" (List.head (List.reverse currentPath)) ++ ": "

        elementId =
            pathToElementId pathKey
    in
    div [ id elementId, class ("inline-flex items-center gap-1 py-0.5 px-1 rounded" ++ highlightClass) ]
        [ if not (String.isEmpty label) then
            span [ class "text-secondary" ] [ text label ]

          else
            text ""
        , viewPrimitiveValue jsonValue
        ]


{-| Render the tree view with diff highlighting.

This renders a custom tree that highlights changed values based on the
provided changedPaths. When `changesOnly` is true, only paths that have
changed (and their parents) are shown. Skips the redundant root object wrapper.

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
        [ viewDiffRootChildren config expandedPaths visiblePaths jsonValue
        ]


{-| Render the children of the root object directly for diff view.
-}
viewDiffRootChildren : DiffConfig msg -> Set String -> Set String -> D.Value -> Html msg
viewDiffRootChildren config expandedPaths visiblePaths jsonValue =
    let
        keys =
            getObjectKeysWithoutType jsonValue

        -- Filter keys to only show those with changes when changesOnly is enabled
        visibleKeys =
            if config.changesOnly then
                List.filter
                    (\key ->
                        Set.member key visiblePaths
                    )
                    keys

            else
                keys
    in
    div [ class "pl-2" ]
        (List.map
            (\key ->
                let
                    childPath =
                        [ key ]

                    childValue =
                        getObjectField key jsonValue
                            |> Maybe.withDefault E.null
                in
                viewDiffKeyValue config expandedPaths visiblePaths childPath key childValue
            )
            visibleKeys
        )


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

        -- Diff highlight class based on change type
        diffClass =
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

        -- Search highlight class
        searchClass =
            searchHighlightClass config.searchMatches config.currentMatchPath pathKey

        -- Combine both classes
        highlightClass =
            diffClass ++ searchClass
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
            getObjectKeysWithoutType jsonValue

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

        hasChildren =
            not (List.isEmpty visibleKeys)

        typeAnnotation =
            getTypeAnnotation jsonValue

        label =
            if List.isEmpty currentPath then
                "root"

            else
                Maybe.withDefault "?" (List.head (List.reverse currentPath))

        elementId =
            pathToElementId pathKey

        labelParts =
            case typeAnnotation of
                Just typeName ->
                    [ span [ class "text-primary" ] [ text label ]
                    , span [ class "text-base-content/40" ] [ text " : " ]
                    , span [ class "text-info" ] [ text typeName ]
                    ]

                Nothing ->
                    [ span [ class "text-primary" ] [ text label ] ]

        arrowAndInteraction =
            if hasChildren then
                ( [ span [ class "text-base-content/40 w-4" ]
                        [ text
                            (if isExpanded then
                                "▼"

                             else
                                "▶"
                            )
                        ]
                  ]
                , [ class ("inline-flex items-center gap-1 cursor-pointer hover:bg-base-200 rounded px-1 py-0.5" ++ highlightClass)
                  , onClick (config.onToggleExpand currentPath)
                  ]
                )

            else
                ( [ span [ class "text-base-content/40 w-4" ] [ text "" ] ]
                , [ class ("inline-flex items-center gap-1 rounded px-1 py-0.5" ++ highlightClass) ]
                )

        ( arrowPart, interactionAttrs ) =
            arrowAndInteraction
    in
    div [ class "tree-node pl-2" ]
        [ div
            ([ id elementId ] ++ interactionAttrs)
            (arrowPart ++ labelParts)
        , if isExpanded && hasChildren then
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

        elementId =
            pathToElementId pathKey
    in
    div [ class "tree-node pl-2" ]
        [ div
            [ id elementId
            , class ("inline-flex items-center gap-1 cursor-pointer hover:bg-base-200 rounded px-1 py-0.5" ++ highlightClass)
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

        diffClass =
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

        searchClass =
            searchHighlightClass config.searchMatches config.currentMatchPath pathKey

        highlightClass =
            diffClass ++ searchClass

        elementId =
            pathToElementId pathKey
    in
    case getValueType childValue of
        ObjectType ->
            viewDiffObject config expandedPaths visiblePaths currentPath childValue highlightClass

        ArrayType ->
            viewDiffArray config expandedPaths visiblePaths currentPath childValue highlightClass

        _ ->
            div [ class "tree-node pl-2" ]
                [ div [ id elementId, class ("inline-flex items-center gap-1 py-0.5 px-1 rounded" ++ highlightClass) ]
                    [ span [ class "text-base-content/40 w-4" ] [ text "" ]
                    , span [ class "text-secondary" ] [ text (key ++ ":") ]
                    , viewPrimitiveValue childValue
                    ]
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

        diffClass =
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

        searchClass =
            searchHighlightClass config.searchMatches config.currentMatchPath pathKey

        highlightClass =
            diffClass ++ searchClass

        elementId =
            pathToElementId pathKey
    in
    case getValueType childValue of
        ObjectType ->
            viewDiffObject config expandedPaths visiblePaths currentPath childValue highlightClass

        ArrayType ->
            viewDiffArray config expandedPaths visiblePaths currentPath childValue highlightClass

        _ ->
            div [ class "tree-node pl-2" ]
                [ div [ id elementId, class ("inline-flex items-center gap-1 py-0.5 px-1 rounded" ++ highlightClass) ]
                    [ span [ class "text-base-content/40 w-4" ] [ text "" ]
                    , span [ class "text-base-content/40" ] [ text (String.fromInt idx ++ ":") ]
                    , viewPrimitiveValue childValue
                    ]
                ]


{-| Render a primitive value (string, number, bool, null) without a key.
-}
viewDiffPrimitive : List String -> D.Value -> String -> Html msg
viewDiffPrimitive currentPath jsonValue highlightClass =
    let
        pathKey =
            String.join "." currentPath

        label =
            if List.isEmpty currentPath then
                ""

            else
                Maybe.withDefault "" (List.head (List.reverse currentPath)) ++ ": "

        elementId =
            pathToElementId pathKey
    in
    div [ id elementId, class ("inline-flex items-center gap-1 py-0.5 px-1 rounded" ++ highlightClass) ]
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


{-| Get the _type field value from an object, if present.
-}
getTypeAnnotation : D.Value -> Maybe String
getTypeAnnotation value =
    case D.decodeValue (D.field "_type" D.string) value of
        Ok typeStr ->
            Just typeStr

        Err _ ->
            Nothing


{-| Get object keys excluding _type.
-}
getObjectKeysWithoutType : D.Value -> List String
getObjectKeysWithoutType value =
    getObjectKeys value
        |> List.filter (\k -> k /= "_type")


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
