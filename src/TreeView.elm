module TreeView exposing
    ( State
    , Config
    , Msg
    , init
    , update
    , view
    , viewEmpty
    , parseValue
    , expandAll
    , collapseAll
    )

{-| Tree view component for displaying JSON state in the TeaForge Debugger.

This module wraps the elm-json-tree-view library to provide a collapsible/expandable
tree view for model state visualization. It manages expand/collapse state and
provides convenience functions for bulk operations.

@docs State, Config, Msg
@docs init, update, view, viewEmpty
@docs parseValue, expandAll, collapseAll

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as D
import JsonTree


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
