module StateView exposing
    ( Config
    , view
    , viewTabs
    , viewContent
    , viewSearchControls
    )

{-| State view component for the TeaForge Debugger.

This module provides the state view with view mode tabs (post-state, split, diff)
and renders the appropriate content based on the selected view mode. It handles
the tab bar, search controls, and dispatches to the correct view implementation.

@docs Config, view, viewTabs, viewContent, viewSearchControls

-}

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Set exposing (Set)
import TreeView
import Types exposing (..)


{-| Configuration for the state view component.

  - `viewMode`: The currently active view mode
  - `onViewModeChange`: Callback when user changes the view mode
  - `treeViewState`: Current state of the tree view component (for after state)
  - `treeViewConfig`: Configuration for the tree view (message wrapping, etc.)
  - `beforeTreeViewState`: Current state of the before tree view (for split view)
  - `beforeTreeViewConfig`: Configuration for the before tree view
  - `beforeState`: The model state before processing (for split/diff views)
  - `afterState`: The model state after processing (always shown)
  - `searchQuery`: Current search query string
  - `onSearchQueryChange`: Callback when search query changes
  - `searchMatches`: List of paths matching the search
  - `currentMatchIndex`: Index of currently highlighted match
  - `onNextMatch`: Callback to navigate to next match
  - `onPreviousMatch`: Callback to navigate to previous match
  - `filterActive`: Whether filter mode is active
  - `onToggleFilter`: Callback to toggle filter mode
  - `changedPaths`: List of paths that changed (for diff view)
  - `changes`: Dictionary mapping path strings to change types (for diff highlighting)
  - `diffExpandedPaths`: Set of expanded path keys in diff view
  - `onDiffToggleExpand`: Callback when expanding/collapsing a node in diff view
  - `hasSelection`: Whether a message is currently selected
  - `isFirstMessage`: Whether this is the first message (no before state)

-}
type alias Config msg =
    { viewMode : ViewMode
    , onViewModeChange : ViewMode -> msg
    , treeViewState : TreeView.State
    , treeViewConfig : TreeView.Config msg
    , beforeTreeViewState : TreeView.State
    , beforeTreeViewConfig : TreeView.Config msg
    , beforeState : Maybe D.Value
    , afterState : Maybe D.Value
    , searchQuery : String
    , onSearchQueryChange : String -> msg
    , searchMatches : List TreePath
    , currentMatchIndex : Int
    , onNextMatch : msg
    , onPreviousMatch : msg
    , filterActive : Bool
    , onToggleFilter : msg
    , changedPaths : List TreePath
    , changes : Dict String TreeView.Change
    , diffExpandedPaths : Set String
    , onDiffToggleExpand : List String -> msg
    , hasSelection : Bool
    , isFirstMessage : Bool
    }


{-| Render the complete state view including tabs and content.

This is the main view function that renders:

  - The tab bar for switching between view modes
  - The search controls
  - The state content based on the current view mode

-}
view : Config msg -> Html msg
view config =
    div [ class "flex-1 flex flex-col overflow-hidden" ]
        [ viewTabBar config
        , viewContent config
        ]


{-| Render the tab bar with view mode tabs and search controls.

The tab bar contains:

  - Three view mode tabs (State, Split, Diff)
  - Search input box
  - Search navigation buttons with match counter
  - Filter toggle

-}
viewTabBar : Config msg -> Html msg
viewTabBar config =
    div [ class "border-b border-base-300" ]
        [ div [ class "flex items-center justify-between px-4 py-2" ]
            [ viewTabs config
            , viewSearchControls config
            ]
        ]


{-| Render just the view mode tabs.

Use this if you need to place the tabs separately from the search controls.

-}
viewTabs : Config msg -> Html msg
viewTabs config =
    div [ role "tablist", class "tabs tabs-bordered" ]
        [ viewModeTab config PostState "State"
        , viewModeTab config SplitView "Split"
        , viewDiffTab config
        ]


{-| Render a single view mode tab button.
-}
viewModeTab : Config msg -> ViewMode -> String -> Html msg
viewModeTab config targetMode label =
    let
        isActive =
            isSameMode config.viewMode targetMode
    in
    a
        [ role "tab"
        , class
            ("tab"
                ++ (if isActive then
                        " tab-active"

                    else
                        ""
                   )
            )
        , onClick (config.onViewModeChange targetMode)
        ]
        [ text label ]


{-| Render the diff tab with optional "changes only" toggle.

The diff tab shows additional controls when active:

  - The tab label
  - A checkbox to toggle "changes only" filter mode

-}
viewDiffTab : Config msg -> Html msg
viewDiffTab config =
    let
        isActive =
            case config.viewMode of
                DiffView _ ->
                    True

                _ ->
                    False

        changesOnly =
            case config.viewMode of
                DiffView { changesOnly } ->
                    changesOnly

                _ ->
                    False
    in
    div [ class "flex items-center" ]
        [ a
            [ role "tab"
            , class
                ("tab"
                    ++ (if isActive then
                            " tab-active"

                        else
                            ""
                       )
                )
            , onClick (config.onViewModeChange (DiffView { changesOnly = changesOnly }))
            ]
            [ text "Diff" ]
        , if isActive then
            label [ class "label cursor-pointer gap-1 ml-2" ]
                [ span [ class "label-text text-xs" ] [ text "Changes only" ]
                , input
                    [ type_ "checkbox"
                    , class "checkbox checkbox-xs"
                    , checked changesOnly
                    , onClick (config.onViewModeChange (DiffView { changesOnly = not changesOnly }))
                    ]
                    []
                ]

          else
            text ""
        ]


{-| Check if two view modes are the same (ignoring DiffView options).
-}
isSameMode : ViewMode -> ViewMode -> Bool
isSameMode mode1 mode2 =
    case ( mode1, mode2 ) of
        ( PostState, PostState ) ->
            True

        ( SplitView, SplitView ) ->
            True

        ( DiffView _, DiffView _ ) ->
            True

        _ ->
            False


{-| Render the search controls including input, navigation, and filter toggle.
-}
viewSearchControls : Config msg -> Html msg
viewSearchControls config =
    div [ class "flex items-center gap-2" ]
        [ div [ class "form-control" ]
            [ input
                [ type_ "text"
                , placeholder "Search..."
                , class "input input-bordered input-sm w-48"
                , value config.searchQuery
                , onInput config.onSearchQueryChange
                ]
                []
            ]
        , viewSearchNavigation config
        , label [ class "label cursor-pointer gap-2" ]
            [ span [ class "label-text text-sm" ] [ text "Filter" ]
            , input
                [ type_ "checkbox"
                , class "toggle toggle-sm"
                , checked config.filterActive
                , onClick config.onToggleFilter
                ]
                []
            ]
        ]


{-| Render search navigation buttons and match counter.
-}
viewSearchNavigation : Config msg -> Html msg
viewSearchNavigation config =
    let
        matchCount =
            List.length config.searchMatches

        hasMatches =
            matchCount > 0 && not (String.isEmpty config.searchQuery)
    in
    div [ class "flex items-center gap-1" ]
        [ button
            [ class "btn btn-ghost btn-xs"
            , onClick config.onPreviousMatch
            , disabled (not hasMatches)
            ]
            [ text "◀" ]
        , if String.isEmpty config.searchQuery then
            span [ class "text-sm text-base-content/60 w-16 text-center" ] [ text "" ]

          else
            span [ class "badge badge-sm w-16" ]
                [ text
                    (String.fromInt (config.currentMatchIndex + 1)
                        ++ " of "
                        ++ String.fromInt matchCount
                    )
                ]
        , button
            [ class "btn btn-ghost btn-xs"
            , onClick config.onNextMatch
            , disabled (not hasMatches)
            ]
            [ text "▶" ]
        ]


{-| Render the state content area based on the current view mode.

Dispatches to the appropriate view:

  - `PostState`: Shows the model state after processing
  - `SplitView`: Shows before and after states side by side
  - `DiffView`: Shows after state with changes highlighted

-}
viewContent : Config msg -> Html msg
viewContent config =
    div [ class "flex-1 overflow-auto p-4" ]
        [ if not config.hasSelection then
            viewNoSelection

          else
            case config.viewMode of
                PostState ->
                    viewPostState config

                SplitView ->
                    viewSplitView config

                DiffView opts ->
                    viewDiffView config opts
        ]


{-| Render placeholder when no message is selected.
-}
viewNoSelection : Html msg
viewNoSelection =
    div [ class "h-full flex items-center justify-center" ]
        [ div [ class "text-center text-base-content/60" ]
            [ p [ class "text-lg" ] [ text "No message selected" ]
            , p [ class "text-sm mt-2" ] [ text "Select a message from the sidebar to view its state" ]
            ]
        ]


{-| Render the post-state view showing the model state after processing.

This is the simplest view mode, displaying just the resulting state
in a collapsible tree format.

-}
viewPostState : Config msg -> Html msg
viewPostState config =
    div [ class "bg-base-100 rounded-lg border border-base-300 p-4 h-full overflow-auto" ]
        [ TreeView.view config.treeViewConfig config.treeViewState
        ]


{-| Render the split view showing before and after states side by side.

This view displays two tree views:

  - Left panel: State before processing the message
  - Right panel: State after processing the message

Both panels use independent TreeView states to allow separate expand/collapse
operations. The first message has no "before" state and shows a placeholder.

-}
viewSplitView : Config msg -> Html msg
viewSplitView config =
    div [ class "grid grid-cols-2 gap-4 h-full" ]
        [ -- Before (left) panel
          div [ class "bg-base-100 rounded-lg border border-base-300 flex flex-col overflow-hidden" ]
            [ div [ class "flex items-center justify-between px-4 py-3 border-b border-base-300 bg-base-200/50" ]
                [ h3 [ class "font-semibold text-base" ] [ text "Before" ]
                , span [ class "badge badge-ghost badge-sm" ] [ text "Previous state" ]
                ]
            , div [ class "flex-1 overflow-auto p-4" ]
                [ if config.isFirstMessage then
                    viewInitialStateMessage

                  else
                    TreeView.view config.beforeTreeViewConfig config.beforeTreeViewState
                ]
            ]

        -- After (right) panel
        , div [ class "bg-base-100 rounded-lg border border-base-300 flex flex-col overflow-hidden" ]
            [ div [ class "flex items-center justify-between px-4 py-3 border-b border-base-300 bg-base-200/50" ]
                [ h3 [ class "font-semibold text-base" ] [ text "After" ]
                , span [ class "badge badge-primary badge-sm" ] [ text "Current state" ]
                ]
            , div [ class "flex-1 overflow-auto p-4" ]
                [ TreeView.view config.treeViewConfig config.treeViewState
                ]
            ]
        ]


{-| Render a message for the initial state (first message has no "before" state).
-}
viewInitialStateMessage : Html msg
viewInitialStateMessage =
    div [ class "flex items-center justify-center h-full" ]
        [ div [ class "text-center text-base-content/60" ]
            [ div [ class "text-4xl mb-3" ] [ text "🎬" ]
            , p [ class "text-sm font-medium" ] [ text "Initial State" ]
            , p [ class "text-xs mt-1" ] [ text "This is the first message - no previous state available" ]
            ]
        ]


{-| Render the diff view showing changes highlighted.

This view displays the after state with changed values visually distinguished.
When `changesOnly` is true, only the changed paths are shown. For the first
message, shows a notice that there's no previous state to compare against.

-}
viewDiffView : Config msg -> { changesOnly : Bool } -> Html msg
viewDiffView config opts =
    let
        changeCount =
            List.length config.changedPaths

        diffConfig =
            { changedPaths = config.changedPaths
            , changes = config.changes
            , changesOnly = opts.changesOnly
            , onToggleExpand = config.onDiffToggleExpand
            }
    in
    div [ class "bg-base-100 rounded-lg border border-base-300 flex flex-col h-full overflow-hidden" ]
        [ -- Header with change count and first message indicator
          div [ class "flex items-center justify-between px-4 py-3 border-b border-base-300 bg-base-200/50" ]
            [ h3 [ class "font-semibold text-base" ] [ text "Diff View" ]
            , div [ class "flex items-center gap-2" ]
                [ if config.isFirstMessage then
                    span [ class "badge badge-info badge-sm" ]
                        [ text "Initial state" ]

                  else if changeCount > 0 then
                    span [ class "badge badge-warning badge-sm" ]
                        [ text (String.fromInt changeCount ++ " changes") ]

                  else
                    span [ class "badge badge-success badge-sm" ]
                        [ text "No changes" ]
                , if opts.changesOnly && not config.isFirstMessage then
                    span [ class "badge badge-info badge-sm" ]
                        [ text "Filtered" ]

                  else
                    text ""
                ]
            ]

        -- Content area
        , div [ class "flex-1 overflow-auto p-4" ]
            [ if config.isFirstMessage then
                viewInitialStateDiffContent config

              else if opts.changesOnly && changeCount == 0 then
                div [ class "flex items-center justify-center h-32" ]
                    [ div [ class "text-center text-base-content/60" ]
                        [ p [ class "text-sm" ] [ text "No changes detected" ]
                        , p [ class "text-xs mt-1" ] [ text "The state is identical before and after this message" ]
                        ]
                    ]

              else
                case config.afterState of
                    Just afterJson ->
                        TreeView.viewDiff diffConfig afterJson config.diffExpandedPaths

                    Nothing ->
                        TreeView.viewEmpty
            ]

        -- Legend (hidden for first message since no diff is meaningful)
        , if config.isFirstMessage then
            text ""

          else
            div [ class "flex items-center gap-4 px-4 py-2 border-t border-base-300 bg-base-200/30 text-xs" ]
                [ span [ class "flex items-center gap-1" ]
                    [ span [ class "w-3 h-3 bg-warning/20 border-l-2 border-warning" ] []
                    , text "Modified"
                    ]
                , span [ class "flex items-center gap-1" ]
                    [ span [ class "w-3 h-3 bg-success/20 border-l-2 border-success" ] []
                    , text "Added"
                    ]
                , span [ class "flex items-center gap-1" ]
                    [ span [ class "w-3 h-3 bg-error/20 border-l-2 border-error" ] []
                    , text "Removed"
                    ]
                ]
        ]


{-| Render the initial state content for diff view when it's the first message.

Shows an informational alert and the current state as a regular tree view.

-}
viewInitialStateDiffContent : Config msg -> Html msg
viewInitialStateDiffContent config =
    div [ class "space-y-4" ]
        [ -- Info message
          div [ class "alert alert-info" ]
            [ div [ class "flex items-center gap-2" ]
                [ span [ class "text-lg" ] [ text "🎬" ]
                , div []
                    [ p [ class "font-medium" ] [ text "Initial State" ]
                    , p [ class "text-sm opacity-80" ] [ text "This is the first message - no previous state to compare against" ]
                    ]
                ]
            ]

        -- Show the current state as a regular tree view
        , div [ class "border border-base-300 rounded-lg p-4 bg-base-100" ]
            [ h4 [ class "text-sm font-medium mb-3 text-base-content/70" ] [ text "Current State:" ]
            , TreeView.view config.treeViewConfig config.treeViewState
            ]
        ]
