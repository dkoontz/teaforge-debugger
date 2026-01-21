module Main exposing (main)

{-| Main entry point for the TeaForge Debugger application.

This module implements The Elm Architecture (TEA) with Model, Msg, init, update,
view, and subscriptions. It provides the application shell and coordinates
communication with JavaScript via ports.

-}

import Browser
import Browser.Dom as Dom
import Browser.Events
import Dict exposing (Dict)
import Diff
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import LogParser exposing (ParseError(..), parseLogFile)
import MessageList
import Ports
import Search
import Set exposing (Set)
import Task
import TreeView
import Types exposing (..)



-- MAIN


{-| Application entry point using Browser.element.

Browser.element is appropriate for this Electron-embedded application as it:

  - Supports subscriptions (needed for port communication)
  - Mounts to a specific DOM node
  - Doesn't require URL handling

-}
main : Program E.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


{-| The application model containing all state.

This structure follows the design from the spec, managing:

  - Log entries loaded from a file
  - Currently selected message index
  - View mode (post-state, split, diff)
  - Search and filter state
  - UI state (sidebar width, loading state)
  - Tree view states for after and before states (split view)
  - Diff view state (expanded paths, computed changes)

-}
type alias Model =
    { logEntries : List LogEntry
    , selectedIndex : Maybe Int
    , viewMode : ViewMode
    , searchQuery : String
    , searchMatches : List TreePath
    , searchPathsWithMatches : Set String
    , currentMatchIndex : Int
    , filterActive : Bool
    , filterExpandedPaths : Set String
    , sidebarWidth : Int
    , loadingState : LoadingState
    , errorMessage : Maybe String
    , skippedEntries : Int
    , treeViewState : TreeView.State
    , beforeTreeViewState : TreeView.State
    , diffExpandedPaths : Set String
    , changedPaths : List TreePath
    , changes : Dict String Diff.Change
    }


{-| Initialize the model with default values.

The application starts in an Idle state with no file loaded.

-}
init : E.Value -> ( Model, Cmd Msg )
init _ =
    ( { logEntries = []
      , selectedIndex = Nothing
      , viewMode = PostState
      , searchQuery = ""
      , searchMatches = []
      , searchPathsWithMatches = Set.empty
      , currentMatchIndex = 0
      , filterActive = False
      , filterExpandedPaths = Set.singleton ""
      , sidebarWidth = 320
      , loadingState = Idle
      , errorMessage = Nothing
      , skippedEntries = 0
      , treeViewState = TreeView.init
      , beforeTreeViewState = TreeView.init
      , diffExpandedPaths = Set.singleton ""
      , changedPaths = []
      , changes = Dict.empty
      }
    , Cmd.none
    )



-- MSG


{-| Messages that can be dispatched in the application.

These are organized into categories:

  - File operations (opening, loading)
  - Navigation (selecting messages)
  - View mode changes
  - Search operations
  - Port responses

-}
type Msg
    = -- File Operations
      OpenFileDialog
    | FileDialogResult { success : Bool, filePath : Maybe String, error : Maybe String }
    | FileReadResult { success : Bool, content : Maybe String, path : String, error : Maybe String }
      -- Navigation
    | SelectMessage Int
      -- View Mode
    | SetViewMode ViewMode
      -- Search
    | SetSearchQuery String
    | NextMatch
    | PreviousMatch
    | ToggleFilter
    | FocusSearch
    | NoOp
      -- Tree View (After state)
    | TreeViewMsg TreeView.Msg
      -- Tree View (Before state - for split view)
    | BeforeTreeViewMsg TreeView.Msg
      -- Diff View
    | DiffToggleExpand (List String)
      -- Filter View
    | FilterToggleExpand (List String)
      -- Port Communication
    | GotPortMessage E.Value
      -- Error Handling
    | DismissError



-- UPDATE


{-| Update function handling all application messages.

Each message handler returns the new model and any commands to execute.

-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- File Operations
        OpenFileDialog ->
            ( { model | loadingState = Loading }
            , Ports.openFileDialog
            )

        FileDialogResult result ->
            if result.success then
                case result.filePath of
                    Just path ->
                        ( model
                        , Ports.readFile path
                        )

                    Nothing ->
                        -- User cancelled the dialog
                        ( { model | loadingState = Idle }
                        , Cmd.none
                        )

            else
                ( { model
                    | loadingState = Error (Maybe.withDefault "Failed to open file dialog" result.error)
                  }
                , Cmd.none
                )

        FileReadResult result ->
            if result.success then
                case result.content of
                    Just content ->
                        case parseLogFile content of
                            Ok ( entries, skippedCount ) ->
                                if List.isEmpty entries && skippedCount == 0 then
                                    ( { model
                                        | loadingState = Error "No log entries found in file"
                                        , logEntries = []
                                        , selectedIndex = Nothing
                                        , skippedEntries = 0
                                      }
                                    , Cmd.none
                                    )

                                else
                                    let
                                        -- Parse the first entry's modelAfter for tree view
                                        initialTreeState =
                                            entries
                                                |> List.head
                                                |> Maybe.map
                                                    (\entry ->
                                                        case TreeView.parseValue entry.modelAfter TreeView.init of
                                                            Ok state ->
                                                                state

                                                            Err _ ->
                                                                TreeView.init
                                                    )
                                                |> Maybe.withDefault TreeView.init

                                        -- Parse the first entry's modelBefore for split view
                                        initialBeforeTreeState =
                                            entries
                                                |> List.head
                                                |> Maybe.map
                                                    (\entry ->
                                                        case TreeView.parseValue entry.modelBefore TreeView.init of
                                                            Ok state ->
                                                                state

                                                            Err _ ->
                                                                TreeView.init
                                                    )
                                                |> Maybe.withDefault TreeView.init
                                    in
                                    ( { model
                                        | loadingState = Loaded
                                        , logEntries = entries
                                        , selectedIndex =
                                            if List.isEmpty entries then
                                                Nothing

                                            else
                                                Just 0
                                        , skippedEntries = skippedCount
                                        , errorMessage =
                                            if skippedCount > 0 then
                                                Just (String.fromInt skippedCount ++ " malformed entries were skipped")

                                            else
                                                Nothing
                                        , treeViewState = initialTreeState
                                        , beforeTreeViewState = initialBeforeTreeState
                                      }
                                    , Cmd.none
                                    )

                            Err parseError ->
                                ( { model
                                    | loadingState = Error (parseErrorToString parseError)
                                    , logEntries = []
                                    , selectedIndex = Nothing
                                    , skippedEntries = 0
                                  }
                                , Cmd.none
                                )

                    Nothing ->
                        ( { model | loadingState = Error "File was empty" }
                        , Cmd.none
                        )

            else
                ( { model
                    | loadingState = Error (Maybe.withDefault "Failed to read file" result.error)
                  }
                , Cmd.none
                )

        -- Navigation
        SelectMessage index ->
            let
                -- Get the selected log entry's modelAfter and parse it for tree view
                maybeEntry =
                    model.logEntries
                        |> List.drop index
                        |> List.head

                newTreeState =
                    maybeEntry
                        |> Maybe.map
                            (\entry ->
                                case TreeView.parseValue entry.modelAfter model.treeViewState of
                                    Ok state ->
                                        state

                                    Err _ ->
                                        -- If parsing fails, keep the default state
                                        TreeView.init
                            )
                        |> Maybe.withDefault TreeView.init

                -- Parse the before state for split view
                newBeforeTreeState =
                    maybeEntry
                        |> Maybe.map
                            (\entry ->
                                case TreeView.parseValue entry.modelBefore model.beforeTreeViewState of
                                    Ok state ->
                                        state

                                    Err _ ->
                                        -- If parsing fails, keep the default state
                                        TreeView.init
                            )
                        |> Maybe.withDefault TreeView.init

                -- Compute diff for the diff view
                diffResult =
                    maybeEntry
                        |> Maybe.map
                            (\entry ->
                                Diff.findChangedPaths entry.modelBefore entry.modelAfter
                            )

                newChangedPaths =
                    diffResult
                        |> Maybe.map .changedPaths
                        |> Maybe.withDefault []

                newChanges =
                    diffResult
                        |> Maybe.map .changes
                        |> Maybe.withDefault Dict.empty

                -- Re-run search if there's an active query
                searchResult =
                    if String.isEmpty model.searchQuery then
                        { matches = [], matchCount = 0, pathsWithMatches = Set.empty }

                    else
                        maybeEntry
                            |> Maybe.map
                                (\entry ->
                                    Search.search model.searchQuery entry.modelAfter
                                )
                            |> Maybe.withDefault { matches = [], matchCount = 0, pathsWithMatches = Set.empty }
            in
            ( { model
                | selectedIndex = Just index
                , treeViewState = newTreeState
                , beforeTreeViewState = newBeforeTreeState
                , changedPaths = newChangedPaths
                , changes = newChanges
                , searchMatches = searchResult.matches
                , searchPathsWithMatches = searchResult.pathsWithMatches
                , currentMatchIndex =
                    if List.isEmpty searchResult.matches then
                        0

                    else
                        -- Try to preserve the current match index, or reset to 0
                        min model.currentMatchIndex (List.length searchResult.matches - 1)
                            |> max 0
                , filterExpandedPaths =
                    -- Update filter expanded paths if searching
                    if String.isEmpty model.searchQuery then
                        model.filterExpandedPaths

                    else
                        Search.buildVisiblePaths searchResult.matches
              }
            , Cmd.none
            )

        -- View Mode
        SetViewMode mode ->
            ( { model | viewMode = mode }
            , Cmd.none
            )

        -- Search
        SetSearchQuery query ->
            let
                -- Get the current modelAfter from selected entry
                maybeAfterState =
                    model.selectedIndex
                        |> Maybe.andThen
                            (\idx ->
                                model.logEntries
                                    |> List.drop idx
                                    |> List.head
                                    |> Maybe.map .modelAfter
                            )

                -- Perform search on the current state
                searchResult =
                    case maybeAfterState of
                        Just afterState ->
                            Search.search query afterState

                        Nothing ->
                            { matches = [], matchCount = 0, pathsWithMatches = Set.empty }
            in
            ( { model
                | searchQuery = query
                , searchMatches = searchResult.matches
                , searchPathsWithMatches = searchResult.pathsWithMatches
                , currentMatchIndex =
                    if List.isEmpty searchResult.matches then
                        0

                    else
                        0
                , filterExpandedPaths =
                    -- Auto-expand paths with matches when filtering
                    if String.isEmpty (String.trim query) then
                        Set.singleton ""

                    else
                        Search.buildVisiblePaths searchResult.matches
              }
            , Cmd.none
            )

        NextMatch ->
            let
                newIndex =
                    if model.currentMatchIndex >= List.length model.searchMatches - 1 then
                        0

                    else
                        model.currentMatchIndex + 1
            in
            ( { model | currentMatchIndex = newIndex }
            , Cmd.none
            )

        PreviousMatch ->
            let
                newIndex =
                    if model.currentMatchIndex <= 0 then
                        List.length model.searchMatches - 1

                    else
                        model.currentMatchIndex - 1
            in
            ( { model | currentMatchIndex = newIndex }
            , Cmd.none
            )

        ToggleFilter ->
            ( { model | filterActive = not model.filterActive }
            , Cmd.none
            )

        FocusSearch ->
            ( model
            , Task.attempt (\_ -> NoOp) (Dom.focus "search-input")
            )

        NoOp ->
            ( model, Cmd.none )

        -- Tree View (After state)
        TreeViewMsg treeMsg ->
            ( { model | treeViewState = TreeView.update treeMsg model.treeViewState }
            , Cmd.none
            )

        -- Tree View (Before state - for split view)
        BeforeTreeViewMsg treeMsg ->
            ( { model | beforeTreeViewState = TreeView.update treeMsg model.beforeTreeViewState }
            , Cmd.none
            )

        -- Diff View
        DiffToggleExpand path ->
            let
                pathKey =
                    String.join "." path

                newExpandedPaths =
                    if Set.member pathKey model.diffExpandedPaths then
                        Set.remove pathKey model.diffExpandedPaths

                    else
                        Set.insert pathKey model.diffExpandedPaths
            in
            ( { model | diffExpandedPaths = newExpandedPaths }
            , Cmd.none
            )

        -- Filter View
        FilterToggleExpand path ->
            let
                pathKey =
                    String.join "." path

                newExpandedPaths =
                    if Set.member pathKey model.filterExpandedPaths then
                        Set.remove pathKey model.filterExpandedPaths

                    else
                        Set.insert pathKey model.filterExpandedPaths
            in
            ( { model | filterExpandedPaths = newExpandedPaths }
            , Cmd.none
            )

        -- Port Communication
        GotPortMessage value ->
            handlePortMessage value model

        -- Error Handling
        DismissError ->
            ( { model | errorMessage = Nothing }
            , Cmd.none
            )


{-| Handle incoming messages from JavaScript via ports.

Decodes the message type and dispatches to the appropriate handler.

-}
handlePortMessage : E.Value -> Model -> ( Model, Cmd Msg )
handlePortMessage value model =
    let
        typeDecoder =
            D.field "type" D.string
    in
    case D.decodeValue typeDecoder value of
        Ok msgType ->
            case msgType of
                "fileDialogResult" ->
                    handleFileDialogResult value model

                "fileReadResult" ->
                    handleFileReadResult value model

                "fileListResult" ->
                    -- Handle file list if needed in future
                    ( model, Cmd.none )

                "error" ->
                    handleErrorResult value model

                _ ->
                    ( model, Cmd.none )

        Err _ ->
            ( { model | errorMessage = Just "Failed to decode port message" }
            , Cmd.none
            )


handleFileDialogResult : E.Value -> Model -> ( Model, Cmd Msg )
handleFileDialogResult value model =
    let
        decoder =
            D.field "payload"
                (D.map3
                    (\success filePath error ->
                        { success = success
                        , filePath = filePath
                        , error = error
                        }
                    )
                    (D.field "success" D.bool)
                    (D.maybe (D.field "filePath" D.string))
                    (D.maybe (D.field "error" D.string))
                )
    in
    case D.decodeValue decoder value of
        Ok result ->
            update (FileDialogResult result) model

        Err _ ->
            ( { model
                | loadingState = Error "Failed to decode file dialog result"
              }
            , Cmd.none
            )


handleFileReadResult : E.Value -> Model -> ( Model, Cmd Msg )
handleFileReadResult value model =
    let
        decoder =
            D.field "payload"
                (D.map4
                    (\success content path error ->
                        { success = success
                        , content = content
                        , path = path
                        , error = error
                        }
                    )
                    (D.field "success" D.bool)
                    (D.maybe (D.field "content" D.string))
                    (D.field "path" D.string)
                    (D.maybe (D.field "error" D.string))
                )
    in
    case D.decodeValue decoder value of
        Ok result ->
            update (FileReadResult result) model

        Err _ ->
            ( { model
                | loadingState = Error "Failed to decode file read result"
              }
            , Cmd.none
            )


handleErrorResult : E.Value -> Model -> ( Model, Cmd Msg )
handleErrorResult value model =
    let
        decoder =
            D.field "payload" (D.field "error" D.string)
    in
    case D.decodeValue decoder value of
        Ok errorMsg ->
            ( { model
                | loadingState = Error errorMsg
                , errorMessage = Just errorMsg
              }
            , Cmd.none
            )

        Err _ ->
            ( { model | errorMessage = Just "An unknown error occurred" }
            , Cmd.none
            )


{-| Convert a ParseError to a user-friendly string message.
-}
parseErrorToString : ParseError -> String
parseErrorToString error =
    case error of
        InvalidJson jsonError ->
            "Invalid JSON format: " ++ jsonError

        UnexpectedFormat message ->
            "Unexpected file format: " ++ message

        EmptyFile ->
            "The file is empty"



-- VIEW


{-| Main view function rendering the application layout.

Uses a flex-based layout with Split.js for resizable panels:

  - Sidebar (split-sidebar) for message list
  - Main content area (split-main) with header, state view, and effects footer
  - Split.js handles the resizable divider between panels

DaisyUI components are used for:

  - Menu for message list
  - Tabs for view mode switching

-}
view : Model -> Html Msg
view model =
    div [ class "flex h-full w-full" ]
        [ -- Sidebar panel (Split.js will manage sizing)
          div
            [ id "split-sidebar"
            , class "flex flex-col h-full overflow-hidden bg-base-200"
            ]
            [ viewSidebar model ]

        -- Main content panel (Split.js will manage sizing)
        , div
            [ id "split-main"
            , class "flex flex-col h-full overflow-hidden"
            ]
            [ viewHeader model
            , viewMainContent model
            ]
        ]


{-| Render the application header with controls.
-}
viewHeader : Model -> Html Msg
viewHeader model =
    header [ class "navbar bg-base-200 border-b border-base-300" ]
        [ div [ class "flex-1" ]
            [ span [ class "text-xl font-bold" ] [ text "TeaForge Debugger" ]
            ]
        , div [ class "flex-none gap-2" ]
            [ viewLoadingIndicator model.loadingState
            , button
                [ class "btn btn-primary btn-sm"
                , onClick OpenFileDialog
                , disabled (model.loadingState == Loading)
                ]
                [ text "Open File" ]
            ]
        ]


{-| Show loading state indicator.
-}
viewLoadingIndicator : LoadingState -> Html Msg
viewLoadingIndicator loadingState =
    case loadingState of
        Loading ->
            span [ class "loading loading-spinner loading-sm" ] []

        Error errMsg ->
            div [ class "tooltip tooltip-left", attribute "data-tip" errMsg ]
                [ span [ class "badge badge-error gap-2" ]
                    [ text "Error" ]
                ]

        _ ->
            text ""


{-| Render the sidebar with message list.

The sidebar is now a flex child of the split panel layout. Split.js manages the width,
so we no longer need to set a hardcoded width style.

-}
viewSidebar : Model -> Html Msg
viewSidebar model =
    div [ class "flex flex-col h-full overflow-hidden" ]
        [ div [ class "p-4 border-b border-base-300 shrink-0" ]
            [ h2 [ class "font-semibold text-lg" ] [ text "Messages" ]
            , div [ class "flex items-center gap-2" ]
                [ span [ class "text-sm text-base-content/60" ]
                    [ text (String.fromInt (List.length model.logEntries) ++ " messages") ]
                , if model.skippedEntries > 0 then
                    div
                        [ class "tooltip tooltip-right"
                        , attribute "data-tip" (String.fromInt model.skippedEntries ++ " malformed entries skipped")
                        ]
                        [ span [ class "badge badge-warning badge-sm" ]
                            [ text (String.fromInt model.skippedEntries ++ " skipped") ]
                        ]

                  else
                    text ""
                ]
            ]
        , div [ class "flex-1 overflow-y-auto" ]
            [ MessageList.view
                { selectedIndex = model.selectedIndex
                , onSelect = SelectMessage
                , entries = model.logEntries
                }
            ]
        ]


{-| Render the main content area.
-}
viewMainContent : Model -> Html Msg
viewMainContent model =
    main_ [ class "flex-1 flex flex-col overflow-hidden" ]
        [ viewViewModeTabs model
        , viewStateContent model
        , viewEffectsFooter model
        ]


{-| Render the view mode tab bar.
-}
viewViewModeTabs : Model -> Html Msg
viewViewModeTabs model =
    div [ class "border-b border-base-300" ]
        [ div [ class "flex items-center justify-between px-4 py-2" ]
            [ div [ role "tablist", class "tabs tabs-bordered" ]
                [ viewModeTab model.viewMode PostState "State"
                , viewModeTab model.viewMode SplitView "Split"
                , viewModeTab model.viewMode (DiffView { changesOnly = False }) "Diff"
                ]
            , viewSearchBox model
            ]
        ]


{-| Render a single view mode tab.
-}
viewModeTab : ViewMode -> ViewMode -> String -> Html Msg
viewModeTab currentMode targetMode label =
    let
        isActive =
            case ( currentMode, targetMode ) of
                ( PostState, PostState ) ->
                    True

                ( SplitView, SplitView ) ->
                    True

                ( DiffView _, DiffView _ ) ->
                    True

                _ ->
                    False
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
        , onClick (SetViewMode targetMode)
        ]
        [ text label ]


{-| Render the search box with navigation controls.

Keyboard shortcuts supported:

  - Enter: Navigate to next match
  - Shift+Enter: Navigate to previous match
  - Cmd/Ctrl+F: Focus search (handled globally in subscriptions)

-}
viewSearchBox : Model -> Html Msg
viewSearchBox model =
    div [ class "flex items-center gap-2" ]
        [ div [ class "form-control" ]
            [ input
                [ type_ "text"
                , id "search-input"
                , placeholder "Search... (⌘F)"
                , class "input input-bordered input-sm w-48"
                , value model.searchQuery
                , onInput SetSearchQuery
                , preventDefaultOn "keydown" searchKeyDecoder
                ]
                []
            ]
        , viewSearchNavigation model
        , label [ class "label cursor-pointer gap-2" ]
            [ span [ class "label-text text-sm" ] [ text "Filter" ]
            , input
                [ type_ "checkbox"
                , class "toggle toggle-sm"
                , checked model.filterActive
                , onClick ToggleFilter
                ]
                []
            ]
        ]


{-| Decoder for keyboard events in the search input.

Handles:

  - Enter: Navigate to next match
  - Shift+Enter: Navigate to previous match

Returns (Msg, Bool) where Bool indicates whether to prevent default behavior.

-}
searchKeyDecoder : D.Decoder ( Msg, Bool )
searchKeyDecoder =
    D.map2
        (\key shiftKey ->
            if key == "Enter" then
                if shiftKey then
                    ( PreviousMatch, True )

                else
                    ( NextMatch, True )

            else
                ( NoOp, False )
        )
        (D.field "key" D.string)
        (D.field "shiftKey" D.bool)


{-| Render search navigation buttons and match counter.
-}
viewSearchNavigation : Model -> Html Msg
viewSearchNavigation model =
    let
        matchCount =
            List.length model.searchMatches

        hasMatches =
            matchCount > 0 && not (String.isEmpty model.searchQuery)

        -- Show "0 of 0" when no matches, "X of Y" when there are matches
        displayIndex =
            if matchCount == 0 then
                0

            else
                model.currentMatchIndex + 1
    in
    div [ class "flex items-center gap-1" ]
        [ button
            [ class "btn btn-ghost btn-xs"
            , onClick PreviousMatch
            , disabled (not hasMatches)
            ]
            [ text "◀" ]
        , if String.isEmpty model.searchQuery then
            span [ class "text-sm text-base-content/60 w-16 text-center" ] [ text "" ]

          else
            span [ class "badge badge-sm w-16" ]
                [ text
                    (String.fromInt displayIndex
                        ++ " of "
                        ++ String.fromInt matchCount
                    )
                ]
        , button
            [ class "btn btn-ghost btn-xs"
            , onClick NextMatch
            , disabled (not hasMatches)
            ]
            [ text "▶" ]
        ]


{-| Render the state content area based on view mode.

Shows a loading indicator when a file is being loaded, otherwise displays
the appropriate state view based on selection.

-}
viewStateContent : Model -> Html Msg
viewStateContent model =
    div [ class "flex-1 overflow-auto p-4" ]
        [ case model.loadingState of
            Loading ->
                viewLoadingState

            _ ->
                case model.selectedIndex of
                    Nothing ->
                        viewNoSelection model

                    Just _ ->
                        viewSelectedState model
        ]


{-| Render loading state placeholder when a file is being loaded.
-}
viewLoadingState : Html Msg
viewLoadingState =
    div [ class "h-full flex items-center justify-center" ]
        [ div [ class "text-center" ]
            [ span [ class "loading loading-spinner loading-lg text-primary" ] []
            , p [ class "text-lg mt-4 text-base-content/80" ] [ text "Loading file..." ]
            , p [ class "text-sm mt-2 text-base-content/60" ] [ text "Parsing log entries" ]
            ]
        ]


{-| Render placeholder when no message is selected.

Shows different messages depending on whether log entries have been loaded.

-}
viewNoSelection : Model -> Html Msg
viewNoSelection model =
    div [ class "h-full flex items-center justify-center" ]
        [ div [ class "text-center text-base-content/60" ]
            (if List.isEmpty model.logEntries then
                -- No file loaded yet
                [ div [ class "text-5xl mb-4" ] [ text "📂" ]
                , p [ class "text-lg font-medium" ] [ text "No file loaded" ]
                , p [ class "text-sm mt-2" ] [ text "Click 'Open File' to load a TeaForge log file" ]
                , button
                    [ class "btn btn-primary btn-sm mt-4"
                    , onClick OpenFileDialog
                    ]
                    [ text "Open File" ]
                ]

             else
                -- File loaded but no message selected
                [ div [ class "text-5xl mb-4" ] [ text "👆" ]
                , p [ class "text-lg font-medium" ] [ text "No message selected" ]
                , p [ class "text-sm mt-2" ] [ text "Select a message from the sidebar to view its state" ]
                ]
            )
        ]


{-| Render the state view for the selected message.

Displays the model state using TreeView component for PostState mode.
SplitView shows before and after states side by side.
DiffView shows changes highlighted with "changes only" filter option.

-}
viewSelectedState : Model -> Html Msg
viewSelectedState model =
    let
        -- Check if this is the first message (index 0)
        isFirstMessage =
            model.selectedIndex == Just 0
    in
    case model.viewMode of
        PostState ->
            viewPostStateMode model

        SplitView ->
            viewSplitMode model isFirstMessage

        DiffView opts ->
            viewDiffMode model opts isFirstMessage


{-| Render the post-state view mode.

When filter is active and there's a search query, shows only matching paths.
Otherwise shows the full tree view.

-}
viewPostStateMode : Model -> Html Msg
viewPostStateMode model =
    let
        -- Get the selected entry's modelAfter for rendering
        maybeAfterState =
            model.selectedIndex
                |> Maybe.andThen
                    (\idx ->
                        model.logEntries
                            |> List.drop idx
                            |> List.head
                            |> Maybe.map .modelAfter
                    )

        -- Should we show filtered view?
        showFiltered =
            model.filterActive && not (String.isEmpty (String.trim model.searchQuery))
    in
    if showFiltered then
        let
            visiblePaths =
                Search.buildVisiblePaths model.searchMatches

            filterConfig =
                { visiblePaths = visiblePaths
                , matchingPaths = model.searchPathsWithMatches
                , onToggleExpand = FilterToggleExpand
                }
        in
        div [ class "bg-base-100 rounded-lg border border-base-300 flex flex-col h-full overflow-hidden" ]
            [ -- Header indicating filtered mode
              div [ class "flex items-center justify-between px-4 py-2 border-b border-base-300 bg-base-200/50" ]
                [ div [ class "flex items-center gap-2" ]
                    [ span [ class "text-sm font-medium" ] [ text "Filtered View" ]
                    , span [ class "badge badge-info badge-sm" ]
                        [ text (String.fromInt (List.length model.searchMatches) ++ " matches") ]
                    ]
                ]

            -- Content area
            , div [ class "flex-1 overflow-auto p-4" ]
                [ case maybeAfterState of
                    Just afterJson ->
                        TreeView.viewFiltered filterConfig afterJson model.filterExpandedPaths

                    Nothing ->
                        TreeView.viewEmpty
                ]
            ]

    else
        div [ class "bg-base-100 rounded-lg border border-base-300 p-4 h-full overflow-auto" ]
            [ TreeView.view
                { onSelect = Nothing
                , toMsg = TreeViewMsg
                }
                model.treeViewState
            ]


{-| Render the split view mode with before and after states side by side.

The before panel shows the model state before processing the selected message.
The after panel shows the model state after processing.
The first message has no "before" state and shows a placeholder.

-}
viewSplitMode : Model -> Bool -> Html Msg
viewSplitMode model isFirstMessage =
    div [ class "grid grid-cols-2 gap-4 h-full" ]
        [ -- Before (left) panel
          div [ class "bg-base-100 rounded-lg border border-base-300 flex flex-col overflow-hidden" ]
            [ div [ class "flex items-center justify-between px-4 py-3 border-b border-base-300 bg-base-200/50" ]
                [ h3 [ class "font-semibold text-base" ] [ text "Before" ]
                , span [ class "badge badge-ghost badge-sm" ] [ text "Previous state" ]
                ]
            , div [ class "flex-1 overflow-auto p-4" ]
                [ if isFirstMessage then
                    viewInitialStateMessage

                  else
                    TreeView.view
                        { onSelect = Nothing
                        , toMsg = BeforeTreeViewMsg
                        }
                        model.beforeTreeViewState
                ]
            ]

        -- After (right) panel
        , div [ class "bg-base-100 rounded-lg border border-base-300 flex flex-col overflow-hidden" ]
            [ div [ class "flex items-center justify-between px-4 py-3 border-b border-base-300 bg-base-200/50" ]
                [ h3 [ class "font-semibold text-base" ] [ text "After" ]
                , span [ class "badge badge-primary badge-sm" ] [ text "Current state" ]
                ]
            , div [ class "flex-1 overflow-auto p-4" ]
                [ TreeView.view
                    { onSelect = Nothing
                    , toMsg = TreeViewMsg
                    }
                    model.treeViewState
                ]
            ]
        ]


{-| Render a message for the initial state (first message has no "before" state).
-}
viewInitialStateMessage : Html Msg
viewInitialStateMessage =
    div [ class "flex items-center justify-center h-full" ]
        [ div [ class "text-center text-base-content/60" ]
            [ div [ class "text-4xl mb-3" ] [ text "🎬" ]
            , p [ class "text-sm font-medium" ] [ text "Initial State" ]
            , p [ class "text-xs mt-1" ] [ text "This is the first message - no previous state available" ]
            ]
        ]


{-| Render the diff view mode with change highlighting.

Shows the model state with changed values highlighted. When `changesOnly` is true,
only paths with changes are displayed. For the first message, shows a notice
that there's no previous state to compare against.

-}
viewDiffMode : Model -> { changesOnly : Bool } -> Bool -> Html Msg
viewDiffMode model opts isFirstMessage =
    let
        changeCount =
            List.length model.changedPaths

        -- Get the selected entry's modelAfter for rendering
        maybeAfterState =
            model.selectedIndex
                |> Maybe.andThen
                    (\idx ->
                        model.logEntries
                            |> List.drop idx
                            |> List.head
                            |> Maybe.map .modelAfter
                    )

        -- Convert Diff.Change to TreeView.Change
        treeViewChanges =
            Dict.map (\_ change -> diffChangeToTreeViewChange change) model.changes

        diffConfig =
            { changedPaths = model.changedPaths
            , changes = treeViewChanges
            , changesOnly = opts.changesOnly
            , onToggleExpand = DiffToggleExpand
            }
    in
    div [ class "bg-base-100 rounded-lg border border-base-300 flex flex-col h-full overflow-hidden" ]
        [ -- Header with change count and first message indicator
          div [ class "flex items-center justify-between px-4 py-3 border-b border-base-300 bg-base-200/50" ]
            [ h3 [ class "font-semibold text-base" ] [ text "Diff View" ]
            , div [ class "flex items-center gap-2" ]
                [ if isFirstMessage then
                    span [ class "badge badge-info badge-sm" ]
                        [ text "Initial state" ]

                  else if changeCount > 0 then
                    span [ class "badge badge-warning badge-sm" ]
                        [ text (String.fromInt changeCount ++ " changes") ]

                  else
                    span [ class "badge badge-success badge-sm" ]
                        [ text "No changes" ]
                , if opts.changesOnly && not isFirstMessage then
                    span [ class "badge badge-info badge-sm" ]
                        [ text "Filtered" ]

                  else
                    text ""
                ]
            ]

        -- Content area
        , div [ class "flex-1 overflow-auto p-4" ]
            [ if isFirstMessage then
                viewInitialStateDiffMessage maybeAfterState model

              else if opts.changesOnly && changeCount == 0 then
                div [ class "flex items-center justify-center h-32" ]
                    [ div [ class "text-center text-base-content/60" ]
                        [ p [ class "text-sm" ] [ text "No changes detected" ]
                        , p [ class "text-xs mt-1" ] [ text "The state is identical before and after this message" ]
                        ]
                    ]

              else
                case maybeAfterState of
                    Just afterJson ->
                        TreeView.viewDiff diffConfig afterJson model.diffExpandedPaths

                    Nothing ->
                        TreeView.viewEmpty
            ]

        -- Legend (hidden for first message since no diff is meaningful)
        , if isFirstMessage then
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


{-| Render the initial state message for diff view when it's the first message.

For the first message, there's no meaningful diff since there's no previous
state to compare against. Shows the initial state with an explanation.

-}
viewInitialStateDiffMessage : Maybe D.Value -> Model -> Html Msg
viewInitialStateDiffMessage maybeAfterState model =
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
            , TreeView.view
                { onSelect = Nothing
                , toMsg = TreeViewMsg
                }
                model.treeViewState
            ]
        ]


{-| Convert a Diff.Change to TreeView.Change.
-}
diffChangeToTreeViewChange : Diff.Change -> TreeView.Change
diffChangeToTreeViewChange change =
    case change of
        Diff.Added ->
            TreeView.Added

        Diff.Removed ->
            TreeView.Removed

        Diff.Modified ->
            TreeView.Modified

        Diff.TypeChanged ->
            TreeView.TypeChanged


{-| Render the effects footer section.

Displays the list of effects (commands) produced by the selected message's
update function. Effects are shown in a collapsible panel with their names
and associated data.

-}
viewEffectsFooter : Model -> Html Msg
viewEffectsFooter model =
    let
        selectedEffects =
            case model.selectedIndex of
                Just index ->
                    model.logEntries
                        |> List.drop index
                        |> List.head
                        |> Maybe.map .effects
                        |> Maybe.withDefault []

                Nothing ->
                    []

        effectCount =
            List.length selectedEffects
    in
    div [ class "border-t border-base-300 bg-base-200" ]
        [ div [ class "collapse collapse-arrow" ]
            [ input [ type_ "checkbox", class "peer" ] []
            , div [ class "collapse-title font-medium" ]
                [ text "Effects"
                , if effectCount > 0 then
                    span [ class "badge badge-primary badge-sm ml-2" ]
                        [ text (String.fromInt effectCount) ]

                  else
                    span [ class "badge badge-ghost badge-sm ml-2" ]
                        [ text "0" ]
                ]
            , div [ class "collapse-content" ]
                [ viewEffectsList selectedEffects ]
            ]
        ]


{-| Render the list of effects or an empty state message.
-}
viewEffectsList : List Effect -> Html Msg
viewEffectsList effects =
    if List.isEmpty effects then
        p [ class "text-sm text-base-content/60 py-2" ]
            [ text "No effects for this message" ]

    else
        ul [ class "space-y-2 pt-2" ]
            (List.indexedMap viewEffectItem effects)


{-| Render a single effect item with its name and data.
-}
viewEffectItem : Int -> Effect -> Html Msg
viewEffectItem index effect =
    li [ class "bg-base-100 rounded-lg border border-base-300 overflow-hidden" ]
        [ div [ class "flex items-center gap-2 px-3 py-2 bg-base-200/50 border-b border-base-300" ]
            [ span [ class "badge badge-outline badge-sm" ]
                [ text (String.fromInt (index + 1)) ]
            , span [ class "font-mono font-medium text-sm" ]
                [ text effect.name ]
            ]
        , div [ class "p-3" ]
            [ viewEffectData effect.data ]
        ]


{-| Render effect data as formatted JSON.

Converts the Json.Decode.Value to a pretty-printed JSON string.
Truncates very long values to keep the UI manageable.

-}
viewEffectData : E.Value -> Html Msg
viewEffectData data =
    let
        jsonString =
            E.encode 2 data

        -- Truncate very long strings for readability
        maxLength =
            500

        displayString =
            if String.length jsonString > maxLength then
                String.left maxLength jsonString ++ "\n... (truncated)"

            else
                jsonString
    in
    pre [ class "text-xs font-mono bg-base-100 p-2 rounded overflow-x-auto whitespace-pre-wrap" ]
        [ text displayString ]



-- SUBSCRIPTIONS


{-| Application subscriptions.

Subscribes to:

  - Incoming port for JavaScript messages
  - Global keyboard events for shortcuts (Cmd/Ctrl+F for search focus)

Note: We use preventDefaultOn to prevent browser's default Cmd/Ctrl+F behavior.

-}
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.incoming GotPortMessage
        , Browser.Events.preventDefaultOn "keydown" keyboardShortcutDecoder
        ]


{-| Decoder for global keyboard shortcuts.

Handles:

  - Cmd/Ctrl+F: Focus the search input (prevents default browser find)

Returns (Msg, Bool) where Bool indicates whether to prevent default behavior.

-}
keyboardShortcutDecoder : D.Decoder ( Msg, Bool )
keyboardShortcutDecoder =
    D.map4
        (\key metaKey ctrlKey tagName ->
            -- Cmd/Ctrl+F: Focus search input
            if key == "f" && (metaKey || ctrlKey) then
                -- Always prevent default for Cmd/Ctrl+F to avoid browser find dialog
                -- Focus search even if already in an input (user might be in a different input)
                ( FocusSearch, True )

            else
                ( NoOp, False )
        )
        (D.field "key" D.string)
        (D.field "metaKey" D.bool)
        (D.field "ctrlKey" D.bool)
        (D.at [ "target", "tagName" ] D.string)
