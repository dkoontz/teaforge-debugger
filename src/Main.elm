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
import Types exposing (DisplayOrder(..), Effect, LoadingState(..), LogEntry, TreePath)



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


{-| State for a single message's tree views.

This structure stores the expanded/collapsed state for all tree views
associated with a particular message. Storing this per-message allows
the UI to retain expansion state when switching between messages.

-}
type alias MessageViewState =
    { beforeExpandedPaths : Set String
    , afterExpandedPaths : Set String
    , effectExpandedPaths : Dict Int (Set String)
    , payloadExpandedPaths : Set String
    }


{-| Create default view state for a message.
-}
defaultMessageViewState : MessageViewState
defaultMessageViewState =
    { beforeExpandedPaths = Set.singleton ""
    , afterExpandedPaths = Set.singleton ""
    , effectExpandedPaths = Dict.empty
    , payloadExpandedPaths = Set.singleton ""
    }


{-| Create view state with paths auto-expanded for changes.
-}
messageViewStateWithChanges : List TreePath -> MessageViewState
messageViewStateWithChanges changedPaths =
    let
        expandedPaths =
            Set.union (Set.singleton "") (Search.buildVisiblePaths changedPaths)
    in
    { beforeExpandedPaths = expandedPaths
    , afterExpandedPaths = expandedPaths
    , effectExpandedPaths = Dict.empty
    , payloadExpandedPaths = Set.singleton ""
    }


{-| Get the view state for the currently selected message.

Returns the default view state if no message is selected or no state exists.

-}
getMessageViewState : Model -> MessageViewState
getMessageViewState model =
    case model.selectedIndex of
        Just idx ->
            Dict.get idx model.messageViewStates
                |> Maybe.withDefault defaultMessageViewState

        Nothing ->
            defaultMessageViewState


{-| Update the view state for the currently selected message.

If no message is selected, returns the model unchanged.

-}
updateMessageViewState : (MessageViewState -> MessageViewState) -> Model -> Model
updateMessageViewState updateFn model =
    case model.selectedIndex of
        Just idx ->
            let
                currentState =
                    Dict.get idx model.messageViewStates
                        |> Maybe.withDefault defaultMessageViewState

                newState =
                    updateFn currentState

                newMessageViewStates =
                    Dict.insert idx newState model.messageViewStates
            in
            { model | messageViewStates = newMessageViewStates }

        Nothing ->
            model


{-| The application model containing all state.

This structure follows the design from the spec, managing:

  - Log entries loaded from a file
  - Currently selected message index
  - View options (show previous state, highlight changes)
  - Search and filter state
  - UI state (sidebar width, loading state)
  - Tree view states for after and before states
  - Diff view state (expanded paths, computed changes)
  - Per-message view states for retaining expansion state

-}
type alias Model =
    { logEntries : List LogEntry
    , selectedIndex : Maybe Int
    , displayOrder : DisplayOrder
    , showPreviousState : Bool
    , showChangedValues : Bool
    , searchQuery : String
    , searchResult : Search.EntrySearchResult
    , currentMatchIndex : Int
    , filterActive : Bool
    , filterExpandedPaths : Set String
    , sidebarWidth : Int
    , isResizingSidebar : Bool
    , loadingState : LoadingState
    , errorMessage : Maybe String
    , skippedEntries : Int
    , treeViewState : TreeView.State
    , beforeTreeViewState : TreeView.State
    , changedPaths : List TreePath
    , changes : Dict String Diff.Change
    , messageViewStates : Dict Int MessageViewState
    }


{-| Flags passed from JavaScript on initialization.
-}
type alias Flags =
    { sidebarWidth : Int
    }


{-| Decode flags from JavaScript.
-}
flagsDecoder : D.Decoder Flags
flagsDecoder =
    D.map Flags
        (D.field "sidebarWidth" D.int)


{-| Initialize the model with default values.

The application starts in an Idle state with no file loaded.
Sidebar width is loaded from localStorage via flags.

-}
init : E.Value -> ( Model, Cmd Msg )
init flagsValue =
    let
        initialSidebarWidth =
            case D.decodeValue flagsDecoder flagsValue of
                Ok flags ->
                    clamp 200 600 flags.sidebarWidth

                Err _ ->
                    320
    in
    ( { logEntries = []
      , selectedIndex = Nothing
      , displayOrder = ReverseChronological
      , showPreviousState = False
      , showChangedValues = True
      , searchQuery = ""
      , searchResult = Search.emptyEntrySearchResult
      , currentMatchIndex = 0
      , filterActive = False
      , filterExpandedPaths = Set.singleton ""
      , sidebarWidth = initialSidebarWidth
      , isResizingSidebar = False
      , loadingState = Idle
      , errorMessage = Nothing
      , skippedEntries = 0
      , treeViewState = TreeView.init
      , beforeTreeViewState = TreeView.init
      , changedPaths = []
      , changes = Dict.empty
      , messageViewStates = Dict.empty
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
    | SelectNextMessage
    | SelectPreviousMessage
      -- View Options
    | ToggleShowPreviousState
    | ToggleShowChangedValues
    | SetDisplayOrder DisplayOrder
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
      -- Unified View (before/after without diff highlighting)
    | BeforeToggleExpand (List String)
    | AfterToggleExpand (List String)
      -- Filter View
    | FilterToggleExpand (List String)
      -- Collapse/Expand All
    | CollapseAllBefore
    | ExpandAllBefore
    | CollapseAllAfter
    | ExpandAllAfter
    | CollapseAllFilter
    | ExpandAllFilter
      -- Effect Tree View
    | EffectToggleExpand Int (List String)
    | CollapseAllEffect Int
    | ExpandAllEffect Int
      -- Message Payload Tree View
    | PayloadToggleExpand (List String)
    | CollapseAllPayload
    | ExpandAllPayload
      -- Port Communication
    | GotPortMessage E.Value
      -- Error Handling
    | DismissError
      -- Sidebar Resize
    | StartSidebarResize
    | ResizeSidebar Int
    | StopSidebarResize



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
                                        -- Select the most recent (last) entry by default
                                        lastEntry =
                                            entries
                                                |> List.reverse
                                                |> List.head

                                        lastIndex =
                                            List.length entries - 1

                                        -- Parse the last entry's modelAfter for tree view
                                        initialTreeState =
                                            lastEntry
                                                |> Maybe.map
                                                    (\entry ->
                                                        case TreeView.parseValue entry.modelAfter TreeView.init of
                                                            Ok state ->
                                                                state

                                                            Err _ ->
                                                                TreeView.init
                                                    )
                                                |> Maybe.withDefault TreeView.init

                                        -- Parse the last entry's modelBefore for split view
                                        initialBeforeTreeState =
                                            lastEntry
                                                |> Maybe.map
                                                    (\entry ->
                                                        case TreeView.parseValue entry.modelBefore TreeView.init of
                                                            Ok state ->
                                                                state

                                                            Err _ ->
                                                                TreeView.init
                                                    )
                                                |> Maybe.withDefault TreeView.init

                                        -- Compute diff for the last entry
                                        diffResult =
                                            lastEntry
                                                |> Maybe.map
                                                    (\entry ->
                                                        Diff.findChangedPaths entry.modelBefore entry.modelAfter
                                                    )

                                        initialChangedPaths =
                                            diffResult
                                                |> Maybe.map .changedPaths
                                                |> Maybe.withDefault []

                                        initialChanges =
                                            diffResult
                                                |> Maybe.map .changes
                                                |> Maybe.withDefault Dict.empty

                                        -- Create initial view state with auto-expanded changed paths
                                        initialMessageViewState =
                                            messageViewStateWithChanges initialChangedPaths

                                        -- Store the initial view state for the last entry
                                        initialMessageViewStates =
                                            Dict.singleton lastIndex initialMessageViewState
                                    in
                                    ( { model
                                        | loadingState = Loaded
                                        , logEntries = entries
                                        , selectedIndex =
                                            if List.isEmpty entries then
                                                Nothing

                                            else
                                                Just lastIndex
                                        , skippedEntries = skippedCount
                                        , errorMessage =
                                            if skippedCount > 0 then
                                                Just (String.fromInt skippedCount ++ " malformed entries were skipped")

                                            else
                                                Nothing
                                        , treeViewState = initialTreeState
                                        , beforeTreeViewState = initialBeforeTreeState
                                        , changedPaths = initialChangedPaths
                                        , changes = initialChanges
                                        , messageViewStates = initialMessageViewStates
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
                newSearchResult =
                    if String.isEmpty model.searchQuery then
                        Search.emptyEntrySearchResult

                    else
                        maybeEntry
                            |> Maybe.map (Search.searchEntry model.searchQuery)
                            |> Maybe.withDefault Search.emptyEntrySearchResult

                -- Get or create the view state for this message
                -- If we've never visited this message before, create initial state
                -- with auto-expanded changed paths (when showChangedValues is true)
                existingViewState =
                    Dict.get index model.messageViewStates

                messageViewState =
                    case existingViewState of
                        Just state ->
                            -- Use existing state (preserves user's expand/collapse choices)
                            state

                        Nothing ->
                            -- First time viewing this message - create initial state
                            if model.showChangedValues then
                                messageViewStateWithChanges newChangedPaths

                            else
                                defaultMessageViewState

                -- Update the messageViewStates dict with the current state
                newMessageViewStates =
                    Dict.insert index messageViewState model.messageViewStates
            in
            ( { model
                | selectedIndex = Just index
                , treeViewState = newTreeState
                , beforeTreeViewState = newBeforeTreeState
                , changedPaths = newChangedPaths
                , changes = newChanges
                , messageViewStates = newMessageViewStates
                , searchResult = newSearchResult
                , currentMatchIndex =
                    if newSearchResult.totalMatchCount == 0 then
                        0

                    else
                        -- Try to preserve the current match index, or reset to 0
                        Basics.min model.currentMatchIndex (newSearchResult.totalMatchCount - 1)
                            |> Basics.max 0
                , filterExpandedPaths =
                    -- Update filter expanded paths if searching
                    if String.isEmpty model.searchQuery then
                        model.filterExpandedPaths

                    else
                        Search.buildVisiblePaths newSearchResult.afterMatches
              }
            , Cmd.none
            )

        -- Keyboard Navigation
        -- "Next" and "Previous" are defined in terms of visual direction on screen:
        -- - Next = visually down (ArrowDown)
        -- - Previous = visually up (ArrowUp)
        -- The displayOrder determines how this maps to data indices.
        -- For ReverseChronological: view reverses the list, so display position 0 = highest index
        -- Moving down visually means going to lower display positions, which means lower indices.
        SelectNextMessage ->
            let
                maxIndex =
                    List.length model.logEntries - 1

                -- For ReverseChronological: display is reversed, higher indices at top
                --   Down = toward lower display positions = toward lower indices
                -- For Chronological: display matches data order
                --   Down = toward higher display positions = toward higher indices
                newIndex =
                    case ( model.displayOrder, model.selectedIndex ) of
                        ( ReverseChronological, Just idx ) ->
                            Basics.max (idx - 1) 0

                        ( ReverseChronological, Nothing ) ->
                            -- If nothing selected, select the visually first (top) entry
                            maxIndex

                        ( Chronological, Just idx ) ->
                            Basics.min (idx + 1) maxIndex

                        ( Chronological, Nothing ) ->
                            -- If nothing selected, select the visually first (top) entry
                            0
            in
            if List.isEmpty model.logEntries then
                ( model, Cmd.none )

            else
                let
                    ( newModel, cmd ) =
                        update (SelectMessage newIndex) model

                    scrollCmd =
                        Ports.scrollIntoView ("message-item-" ++ String.fromInt newIndex)

                    focusCmd =
                        Ports.focusElement ("message-item-" ++ String.fromInt newIndex)
                in
                ( newModel, Cmd.batch [ cmd, scrollCmd, focusCmd ] )

        SelectPreviousMessage ->
            let
                maxIndex =
                    List.length model.logEntries - 1

                -- For ReverseChronological: display is reversed, higher indices at top
                --   Up = toward higher display positions = toward higher indices
                -- For Chronological: display matches data order
                --   Up = toward lower display positions = toward lower indices
                newIndex =
                    case ( model.displayOrder, model.selectedIndex ) of
                        ( ReverseChronological, Just idx ) ->
                            Basics.min (idx + 1) maxIndex

                        ( ReverseChronological, Nothing ) ->
                            -- If nothing selected, select the visually last (bottom) entry
                            0

                        ( Chronological, Just idx ) ->
                            Basics.max (idx - 1) 0

                        ( Chronological, Nothing ) ->
                            -- If nothing selected, select the visually last (bottom) entry
                            maxIndex
            in
            if List.isEmpty model.logEntries then
                ( model, Cmd.none )

            else
                let
                    ( newModel, cmd ) =
                        update (SelectMessage newIndex) model

                    scrollCmd =
                        Ports.scrollIntoView ("message-item-" ++ String.fromInt newIndex)

                    focusCmd =
                        Ports.focusElement ("message-item-" ++ String.fromInt newIndex)
                in
                ( newModel, Cmd.batch [ cmd, scrollCmd, focusCmd ] )

        -- View Options
        ToggleShowPreviousState ->
            ( { model | showPreviousState = not model.showPreviousState }
            , Cmd.none
            )

        ToggleShowChangedValues ->
            -- Simply toggle the highlight setting; preserve user's expand/collapse state
            ( { model | showChangedValues = not model.showChangedValues }
            , Cmd.none
            )

        SetDisplayOrder order ->
            ( { model | displayOrder = order }
            , Cmd.none
            )

        -- Search
        SetSearchQuery query ->
            let
                -- Get the current entry for search
                maybeEntry =
                    model.selectedIndex
                        |> Maybe.andThen
                            (\idx ->
                                model.logEntries
                                    |> List.drop idx
                                    |> List.head
                            )

                -- Perform search on the entire entry
                newSearchResult =
                    case maybeEntry of
                        Just entry ->
                            Search.searchEntry query entry

                        Nothing ->
                            Search.emptyEntrySearchResult

                -- Turn off filter when search query is blank
                queryIsBlank =
                    String.isEmpty (String.trim query)
            in
            ( { model
                | searchQuery = query
                , searchResult = newSearchResult
                , currentMatchIndex = 0
                , filterActive =
                    if queryIsBlank then
                        False

                    else
                        model.filterActive
                , filterExpandedPaths =
                    -- Auto-expand paths with matches when filtering
                    if queryIsBlank then
                        Set.singleton ""

                    else
                        Search.buildVisiblePaths newSearchResult.afterMatches
              }
            , Cmd.none
            )

        NextMatch ->
            let
                totalMatches =
                    model.searchResult.totalMatchCount

                newIndex =
                    if totalMatches == 0 then
                        0

                    else if model.currentMatchIndex >= totalMatches - 1 then
                        0

                    else
                        model.currentMatchIndex + 1

                scrollCmd =
                    model.searchResult.allMatches
                        |> List.drop newIndex
                        |> List.head
                        |> Maybe.map (matchLocationToElementId >> Ports.scrollIntoView)
                        |> Maybe.withDefault Cmd.none
            in
            ( { model | currentMatchIndex = newIndex }
            , scrollCmd
            )

        PreviousMatch ->
            let
                totalMatches =
                    model.searchResult.totalMatchCount

                newIndex =
                    if totalMatches == 0 then
                        0

                    else if model.currentMatchIndex <= 0 then
                        totalMatches - 1

                    else
                        model.currentMatchIndex - 1

                scrollCmd =
                    model.searchResult.allMatches
                        |> List.drop newIndex
                        |> List.head
                        |> Maybe.map (matchLocationToElementId >> Ports.scrollIntoView)
                        |> Maybe.withDefault Cmd.none
            in
            ( { model | currentMatchIndex = newIndex }
            , scrollCmd
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

        -- Diff View (uses afterExpandedPaths to share state with non-diff view)
        DiffToggleExpand path ->
            let
                pathKey =
                    String.join "." path

                toggleAfterPath state =
                    { state
                        | afterExpandedPaths =
                            if Set.member pathKey state.afterExpandedPaths then
                                Set.remove pathKey state.afterExpandedPaths

                            else
                                Set.insert pathKey state.afterExpandedPaths
                    }
            in
            ( updateMessageViewState toggleAfterPath model
            , Cmd.none
            )

        -- Unified View (before panel)
        BeforeToggleExpand path ->
            let
                pathKey =
                    String.join "." path

                toggleBeforePath state =
                    { state
                        | beforeExpandedPaths =
                            if Set.member pathKey state.beforeExpandedPaths then
                                Set.remove pathKey state.beforeExpandedPaths

                            else
                                Set.insert pathKey state.beforeExpandedPaths
                    }
            in
            ( updateMessageViewState toggleBeforePath model
            , Cmd.none
            )

        -- Unified View (after panel when not highlighting)
        AfterToggleExpand path ->
            let
                pathKey =
                    String.join "." path

                toggleAfterPath state =
                    { state
                        | afterExpandedPaths =
                            if Set.member pathKey state.afterExpandedPaths then
                                Set.remove pathKey state.afterExpandedPaths

                            else
                                Set.insert pathKey state.afterExpandedPaths
                    }
            in
            ( updateMessageViewState toggleAfterPath model
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

        -- Collapse/Expand All
        CollapseAllBefore ->
            ( updateMessageViewState (\state -> { state | beforeExpandedPaths = Set.singleton "" }) model
            , Cmd.none
            )

        ExpandAllBefore ->
            let
                maybeBeforeState =
                    model.selectedIndex
                        |> Maybe.andThen
                            (\idx ->
                                model.logEntries
                                    |> List.drop idx
                                    |> List.head
                                    |> Maybe.map .modelBefore
                            )

                allPaths =
                    maybeBeforeState
                        |> Maybe.map collectAllPaths
                        |> Maybe.withDefault Set.empty
            in
            ( updateMessageViewState (\state -> { state | beforeExpandedPaths = Set.insert "" allPaths }) model
            , Cmd.none
            )

        CollapseAllAfter ->
            ( updateMessageViewState (\state -> { state | afterExpandedPaths = Set.singleton "" }) model
            , Cmd.none
            )

        ExpandAllAfter ->
            let
                maybeAfterState =
                    model.selectedIndex
                        |> Maybe.andThen
                            (\idx ->
                                model.logEntries
                                    |> List.drop idx
                                    |> List.head
                                    |> Maybe.map .modelAfter
                            )

                allPaths =
                    maybeAfterState
                        |> Maybe.map collectAllPaths
                        |> Maybe.withDefault Set.empty
            in
            ( updateMessageViewState (\state -> { state | afterExpandedPaths = Set.insert "" allPaths }) model
            , Cmd.none
            )

        CollapseAllFilter ->
            ( { model | filterExpandedPaths = Set.singleton "" }
            , Cmd.none
            )

        ExpandAllFilter ->
            let
                maybeAfterState =
                    model.selectedIndex
                        |> Maybe.andThen
                            (\idx ->
                                model.logEntries
                                    |> List.drop idx
                                    |> List.head
                                    |> Maybe.map .modelAfter
                            )

                allPaths =
                    maybeAfterState
                        |> Maybe.map collectAllPaths
                        |> Maybe.withDefault Set.empty
            in
            ( { model | filterExpandedPaths = Set.insert "" allPaths }
            , Cmd.none
            )

        -- Effect Tree View
        EffectToggleExpand effectIndex path ->
            let
                pathKey =
                    String.join "." path

                toggleEffectPath state =
                    let
                        currentPaths =
                            Dict.get effectIndex state.effectExpandedPaths
                                |> Maybe.withDefault (Set.singleton "")

                        newPaths =
                            if Set.member pathKey currentPaths then
                                Set.remove pathKey currentPaths

                            else
                                Set.insert pathKey currentPaths
                    in
                    { state | effectExpandedPaths = Dict.insert effectIndex newPaths state.effectExpandedPaths }
            in
            ( updateMessageViewState toggleEffectPath model
            , Cmd.none
            )

        CollapseAllEffect effectIndex ->
            let
                collapseEffect state =
                    { state | effectExpandedPaths = Dict.insert effectIndex (Set.singleton "") state.effectExpandedPaths }
            in
            ( updateMessageViewState collapseEffect model
            , Cmd.none
            )

        ExpandAllEffect effectIndex ->
            let
                maybeEffectData =
                    model.selectedIndex
                        |> Maybe.andThen
                            (\idx ->
                                model.logEntries
                                    |> List.drop idx
                                    |> List.head
                                    |> Maybe.map .effects
                            )
                        |> Maybe.andThen
                            (\effects ->
                                effects
                                    |> List.drop effectIndex
                                    |> List.head
                            )
                        |> Maybe.map .data

                allPaths =
                    maybeEffectData
                        |> Maybe.map collectAllPaths
                        |> Maybe.withDefault Set.empty

                expandEffect state =
                    { state | effectExpandedPaths = Dict.insert effectIndex (Set.insert "" allPaths) state.effectExpandedPaths }
            in
            ( updateMessageViewState expandEffect model
            , Cmd.none
            )

        -- Message Payload Tree View
        PayloadToggleExpand path ->
            let
                pathKey =
                    String.join "." path

                togglePayloadPath state =
                    let
                        newPaths =
                            if Set.member pathKey state.payloadExpandedPaths then
                                Set.remove pathKey state.payloadExpandedPaths

                            else
                                Set.insert pathKey state.payloadExpandedPaths
                    in
                    { state | payloadExpandedPaths = newPaths }
            in
            ( updateMessageViewState togglePayloadPath model
            , Cmd.none
            )

        CollapseAllPayload ->
            let
                collapsePayload state =
                    { state | payloadExpandedPaths = Set.singleton "" }
            in
            ( updateMessageViewState collapsePayload model
            , Cmd.none
            )

        ExpandAllPayload ->
            let
                maybePayload =
                    model.selectedIndex
                        |> Maybe.andThen
                            (\idx ->
                                model.logEntries
                                    |> List.drop idx
                                    |> List.head
                                    |> Maybe.map (.message >> .payload)
                            )

                allPaths =
                    maybePayload
                        |> Maybe.map collectAllPaths
                        |> Maybe.withDefault Set.empty

                expandPayload state =
                    { state | payloadExpandedPaths = Set.insert "" allPaths }
            in
            ( updateMessageViewState expandPayload model
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

        -- Sidebar Resize
        StartSidebarResize ->
            ( { model | isResizingSidebar = True }
            , Cmd.none
            )

        ResizeSidebar newWidth ->
            let
                clampedWidth =
                    clamp 200 600 newWidth
            in
            ( { model | sidebarWidth = clampedWidth }
            , Ports.saveSidebarWidth clampedWidth
            )

        StopSidebarResize ->
            ( { model | isResizingSidebar = False }
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


{-| Get the path of the current search match for highlighting in tree views.

Returns the path string for the current match, suitable for highlighting in
the relevant tree view. For InMessageName and InEffectName matches, returns
Nothing since those don't have tree paths.

-}
getCurrentMatchPath : Model -> Maybe String
getCurrentMatchPath model =
    model.searchResult.allMatches
        |> List.drop model.currentMatchIndex
        |> List.head
        |> Maybe.andThen matchLocationToPath


{-| Extract the tree path from a MatchLocation, if it has one.
-}
matchLocationToPath : Search.MatchLocation -> Maybe String
matchLocationToPath location =
    case location of
        Search.InMessageName ->
            Nothing

        Search.InMessagePayload path ->
            Just (Search.pathToString path)

        Search.InEffectName _ ->
            Nothing

        Search.InEffectData _ path ->
            Just (Search.pathToString path)

        Search.InModelBefore path ->
            Just (Search.pathToString path)

        Search.InModelAfter path ->
            Just (Search.pathToString path)


{-| Collect all paths from a JSON value for expand all functionality.

Recursively traverses the JSON structure and collects path strings for all
container nodes (objects and arrays).

-}
collectAllPaths : D.Value -> Set String
collectAllPaths value =
    collectAllPathsHelper [] value


collectAllPathsHelper : List String -> D.Value -> Set String
collectAllPathsHelper currentPath value =
    let
        pathKey =
            String.join "." currentPath
    in
    -- Try to decode as object
    case D.decodeValue (D.keyValuePairs D.value) value of
        Ok pairs ->
            -- It's an object - add this path and recurse into children
            List.foldl
                (\( key, childValue ) acc ->
                    Set.union acc (collectAllPathsHelper (currentPath ++ [ key ]) childValue)
                )
                (Set.singleton pathKey)
                pairs

        Err _ ->
            -- Try to decode as array
            case D.decodeValue (D.list D.value) value of
                Ok items ->
                    -- It's an array - add this path and recurse into children
                    List.foldl
                        (\( idx, childValue ) acc ->
                            Set.union acc (collectAllPathsHelper (currentPath ++ [ String.fromInt idx ]) childValue)
                        )
                        (Set.singleton pathKey)
                        (List.indexedMap Tuple.pair items)

                Err _ ->
                    -- It's a primitive - no path to add (only containers can be expanded)
                    Set.empty



-- VIEW


{-| Main view function rendering the application layout.

Uses a flex-based layout with:

  - Fixed-width sidebar for message list
  - Main content area that fills remaining space

DaisyUI components are used for:

  - Menu for message list
  - Tabs for view mode switching

-}
view : Model -> Html Msg
view model =
    div [ class "flex h-full w-full" ]
        [ -- Sidebar panel with controlled width
          div
            [ class "flex flex-col h-full overflow-hidden bg-base-200"
            , style "width" (String.fromInt model.sidebarWidth ++ "px")
            , style "flex-shrink" "0"
            ]
            [ viewSidebar model ]

        -- Resize gutter
        , viewResizeGutter model

        -- Main content panel fills remaining space
        , div
            [ class "flex flex-col h-full overflow-hidden flex-1"
            ]
            [ viewMainContent model
            ]
        ]


{-| Render the resize gutter between sidebar and content.
-}
viewResizeGutter : Model -> Html Msg
viewResizeGutter model =
    div
        [ class "gutter gutter-horizontal"
        , style "flex-shrink" "0"
        , if model.isResizingSidebar then
            class "bg-primary/60"

          else
            class ""
        , onMouseDown StartSidebarResize
        ]
        []


{-| Event handler for mouse down that prevents text selection during resize.
-}
onMouseDown : msg -> Attribute msg
onMouseDown msg =
    Html.Events.preventDefaultOn "mousedown"
        (D.succeed ( msg, True ))


{-| Render the sidebar with message list.

The sidebar is now a flex child of the split panel layout. Split.js manages the width,
so we no longer need to set a hardcoded width style.

-}
viewSidebar : Model -> Html Msg
viewSidebar model =
    div [ class "flex flex-col h-full overflow-hidden min-w-0" ]
        [ div [ class "p-4 border-b border-base-300 shrink-0" ]
            [ h2 [ class "font-semibold text-lg" ] [ text "Messages" ]
            , div [ class "flex flex-nowrap items-center gap-2 mt-1" ]
                [ span [ class "text-sm text-base-content/60 whitespace-nowrap flex-none" ]
                    [ text (String.fromInt (List.length model.logEntries) ++ " messages") ]
                , select
                    [ class "select select-bordered select-xs"
                    , style "width" "auto"
                    , onInput
                        (\val ->
                            if val == "newest" then
                                SetDisplayOrder ReverseChronological

                            else
                                SetDisplayOrder Chronological
                        )
                    ]
                    [ option
                        [ value "newest"
                        , selected (model.displayOrder == ReverseChronological)
                        ]
                        [ text "Newest First" ]
                    , option
                        [ value "oldest"
                        , selected (model.displayOrder == Chronological)
                        ]
                        [ text "Oldest First" ]
                    ]
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
        , div [ class "flex-1 overflow-y-auto overflow-x-hidden min-w-0" ]
            [ MessageList.view
                { selectedIndex = model.selectedIndex
                , onSelect = SelectMessage
                , entries = model.logEntries
                , displayOrder = model.displayOrder
                }
            ]
        ]


{-| Render the main content area.

Layout structure:
  - View options bar (fixed at top)
  - Scrollable container with:
    - Effects panel (accordion, collapsed by default)
    - State display

Both effects and state are in the same scrollable container so users can
scroll to see all content when either is expanded.

-}
viewMainContent : Model -> Html Msg
viewMainContent model =
    main_ [ class "flex-1 flex flex-col overflow-hidden" ]
        [ div [ class "p-4 border-b border-base-300 shrink-0" ]
            [ h2 [ class "font-semibold text-lg" ] [ text "Model" ]
            ]
        , viewViewOptions model
        , div [ class "flex-1 overflow-auto" ]
            [ viewMessageDetailsPanel model
            , viewEffectsPanel model
            , viewStateContent model
            ]
        ]


{-| Render the view options bar with checkboxes.
-}
viewViewOptions : Model -> Html Msg
viewViewOptions model =
    div [ class "border-b border-base-300" ]
        [ div [ class "flex items-center justify-between px-4 py-2" ]
            [ div [ class "flex items-center gap-4" ]
                [ label [ class "label cursor-pointer gap-2" ]
                    [ input
                        [ type_ "checkbox"
                        , class "checkbox checkbox-sm"
                        , checked model.showPreviousState
                        , onClick ToggleShowPreviousState
                        ]
                        []
                    , span [ class "label-text text-sm" ] [ text "Show previous state" ]
                    ]
                , label [ class "label cursor-pointer gap-2" ]
                    [ input
                        [ type_ "checkbox"
                        , class "checkbox checkbox-sm"
                        , checked model.showChangedValues
                        , onClick ToggleShowChangedValues
                        ]
                        []
                    , span [ class "label-text text-sm" ] [ text "Highlight changes" ]
                    ]
                ]
            , viewSearchBox model
            ]
        ]


{-| Render the search box with navigation controls.

Layout: search input → x of y → up/down arrows → filter toggle (all on one line)

Keyboard shortcuts supported:

  - Enter: Navigate to next match
  - Shift+Enter: Navigate to previous match
  - Cmd/Ctrl+F: Focus search (handled globally in subscriptions)

-}
viewSearchBox : Model -> Html Msg
viewSearchBox model =
    let
        matchCount =
            model.searchResult.totalMatchCount

        hasMatches =
            matchCount > 0 && not (String.isEmpty model.searchQuery)

        displayIndex =
            if matchCount == 0 then
                0

            else
                model.currentMatchIndex + 1
    in
    div [ class "flex items-center gap-2" ]
        [ -- Search input
          input
            [ type_ "text"
            , id "search-input"
            , placeholder "Search... (⌘F)"
            , class "input input-sm w-40"
            , value model.searchQuery
            , onInput SetSearchQuery
            , preventDefaultOn "keydown" searchKeyDecoder
            ]
            []

        -- Match counter (x of y)
        , if String.isEmpty model.searchQuery then
            span [ class "text-sm text-base-content/60 w-14 text-center" ] [ text "" ]

          else
            span [ class "badge badge-sm w-14" ]
                [ text
                    (String.fromInt displayIndex
                        ++ " of "
                        ++ String.fromInt matchCount
                    )
                ]

        -- Up/down navigation arrows (horizontal layout)
        , div [ class "flex items-center" ]
            [ div [ class "tooltip tooltip-bottom", attribute "data-tip" "Previous match (Shift+Enter)" ]
                [ button
                    [ class "btn btn-ghost btn-xs px-1"
                    , onClick PreviousMatch
                    , disabled (not hasMatches)
                    ]
                    [ text "▲" ]
                ]
            , div [ class "tooltip tooltip-bottom", attribute "data-tip" "Next match (Enter)" ]
                [ button
                    [ class "btn btn-ghost btn-xs px-1"
                    , onClick NextMatch
                    , disabled (not hasMatches)
                    ]
                    [ text "▼" ]
                ]
            ]

        -- Filter toggle
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


{-| Render the state content area based on view mode.

Shows a loading indicator when a file is being loaded, otherwise displays
the appropriate state view based on selection.

Note: This is now rendered inside a scrollable parent container, so it
doesn't need its own overflow handling.

-}
viewStateContent : Model -> Html Msg
viewStateContent model =
    div [ class "p-4" ]
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

Shows the model state based on view options:
  - If showPreviousState is true, shows before and after side by side
  - If showPreviousState is false, shows only the after state
  - If showChangedValues is true, highlights changes on the after panel

-}
viewSelectedState : Model -> Html Msg
viewSelectedState model =
    let
        -- Check if this is the first message (index 0)
        isFirstMessage =
            model.selectedIndex == Just 0
    in
    if model.showPreviousState then
        viewSplitMode model isFirstMessage

    else
        viewSingleStateMode model


{-| Render the single state view (after state only).

When filter is active and there's a search query, shows only matching paths.
When showChangedValues is true, highlights changes.
Otherwise shows the full tree view.

-}
viewSingleStateMode : Model -> Html Msg
viewSingleStateMode model =
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

        -- Check if this is the first message
        isFirstMessage =
            model.selectedIndex == Just 0

        changeCount =
            List.length model.changedPaths

        -- Get the current message's view state
        viewState =
            getMessageViewState model
    in
    if showFiltered then
        let
            visiblePaths =
                Search.buildVisiblePaths model.searchResult.afterMatches

            currentMatchPath =
                getCurrentMatchPath model

            filterConfig =
                { visiblePaths = visiblePaths
                , matchingPaths = model.searchResult.afterPathsWithMatches
                , currentMatchPath = currentMatchPath
                , onToggleExpand = FilterToggleExpand
                }
        in
        div [ class "bg-base-100 rounded-lg border border-base-300 flex flex-col h-full overflow-hidden" ]
            [ -- Header indicating filtered mode
              div [ class "flex items-center justify-between px-4 py-2 border-b border-base-300 bg-base-200/50" ]
                [ div [ class "flex items-center gap-2" ]
                    [ span [ class "text-sm font-medium" ] [ text "Filtered View" ]
                    , span [ class "badge badge-info badge-sm" ]
                        [ text (String.fromInt model.searchResult.totalMatchCount ++ " matches") ]
                    ]
                , viewCollapseExpandButtons CollapseAllFilter ExpandAllFilter
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

    else if model.showChangedValues && not isFirstMessage then
        -- Show state with diff highlighting
        let
            treeViewChanges =
                Dict.map (\_ change -> diffChangeToTreeViewChange change) model.changes

            currentMatchPath =
                getCurrentMatchPath model

            diffConfig =
                { changedPaths = model.changedPaths
                , changes = treeViewChanges
                , changesOnly = False
                , onToggleExpand = DiffToggleExpand
                , searchMatches = model.searchResult.afterPathsWithMatches
                , currentMatchPath = currentMatchPath
                }
        in
        div [ class "bg-base-100 rounded-lg border border-base-300 flex flex-col h-full overflow-hidden" ]
            [ -- Header with change count
              div [ class "flex items-center justify-between px-4 py-3 border-b border-base-300 bg-base-200/50" ]
                [ h3 [ class "font-semibold text-base" ] [ text "Current State" ]
                , div [ class "flex items-center gap-2" ]
                    [ if changeCount > 0 then
                        span [ class "badge badge-warning badge-sm" ]
                            [ text (String.fromInt changeCount ++ " changes") ]

                      else
                        span [ class "badge badge-success badge-sm" ]
                            [ text "No changes" ]
                    , viewCollapseExpandButtons CollapseAllAfter ExpandAllAfter
                    ]
                ]

            -- Content area
            , div [ class "flex-1 overflow-auto p-4" ]
                [ case maybeAfterState of
                    Just afterJson ->
                        TreeView.viewDiff diffConfig afterJson viewState.afterExpandedPaths

                    Nothing ->
                        TreeView.viewEmpty
                ]

            -- Legend
            , viewDiffLegend
            ]

    else
        let
            currentMatchPath =
                getCurrentMatchPath model
        in
        div [ class "bg-base-100 rounded-lg border border-base-300 flex flex-col h-full overflow-hidden" ]
            [ -- Header
              div [ class "flex items-center justify-between px-4 py-3 border-b border-base-300 bg-base-200/50" ]
                [ h3 [ class "font-semibold text-base" ] [ text "Current State" ]
                , viewCollapseExpandButtons CollapseAllAfter ExpandAllAfter
                ]

            -- Content area
            , div [ class "flex-1 overflow-auto p-4" ]
                [ case maybeAfterState of
                    Just afterJson ->
                        TreeView.viewUnified
                            { onToggleExpand = AfterToggleExpand
                            , searchMatches = model.searchResult.afterPathsWithMatches
                            , currentMatchPath = currentMatchPath
                            }
                            afterJson
                            viewState.afterExpandedPaths

                    Nothing ->
                        TreeView.viewEmpty
                ]
            ]


{-| Render the split view mode with before and after states side by side.

The before panel shows the model state before processing the selected message.
The after panel shows the model state after processing.
When showChangedValues is true, the after panel highlights changes.
When filterActive is true and there's a search query, both panels show only matching fields.
The first message has no "before" state and shows a placeholder.

-}
viewSplitMode : Model -> Bool -> Html Msg
viewSplitMode model isFirstMessage =
    let
        -- Get the selected entry
        maybeEntry =
            model.selectedIndex
                |> Maybe.andThen
                    (\idx ->
                        model.logEntries
                            |> List.drop idx
                            |> List.head
                    )

        -- Get the selected entry's modelAfter for diff rendering
        maybeAfterState =
            maybeEntry |> Maybe.map .modelAfter

        -- Get the selected entry's modelBefore for before panel
        maybeBeforeState =
            maybeEntry |> Maybe.map .modelBefore

        changeCount =
            List.length model.changedPaths

        -- Whether to show diff highlighting on the after panel
        showDiffHighlighting =
            model.showChangedValues && not isFirstMessage

        -- Whether to show filtered view
        showFiltered =
            model.filterActive && not (String.isEmpty (String.trim model.searchQuery))

        -- Get the current message's view state
        viewState =
            getMessageViewState model

        -- Convert Diff.Change to TreeView.Change for the after panel
        treeViewChanges =
            Dict.map (\_ change -> diffChangeToTreeViewChange change) model.changes

        currentMatchPath =
            getCurrentMatchPath model

        diffConfig =
            { changedPaths = model.changedPaths
            , changes = treeViewChanges
            , changesOnly = False
            , onToggleExpand = DiffToggleExpand
            , searchMatches = model.searchResult.afterPathsWithMatches
            , currentMatchPath = currentMatchPath
            }

        -- Config for the before diff view (shows removed items highlighted)
        beforeDiffConfig =
            { changedPaths = model.changedPaths
            , changes = treeViewChanges
            , onToggleExpand = BeforeToggleExpand
            , searchMatches = model.searchResult.beforePathsWithMatches
            , currentMatchPath = currentMatchPath
            }

        -- Filter config for filtered view (after model)
        visiblePathsAfter =
            Search.buildVisiblePaths model.searchResult.afterMatches

        filterConfigAfter =
            { visiblePaths = visiblePathsAfter
            , matchingPaths = model.searchResult.afterPathsWithMatches
            , currentMatchPath = currentMatchPath
            , onToggleExpand = FilterToggleExpand
            }

        -- Filter config for before model
        visiblePathsBefore =
            Search.buildVisiblePaths model.searchResult.beforeMatches

        filterConfigBefore =
            { visiblePaths = visiblePathsBefore
            , matchingPaths = model.searchResult.beforePathsWithMatches
            , currentMatchPath = currentMatchPath
            , onToggleExpand = FilterToggleExpand
            }

        matchCount =
            model.searchResult.totalMatchCount

        -- Unified config for non-diff views
        unifiedConfigBefore =
            { onToggleExpand = BeforeToggleExpand
            , searchMatches = model.searchResult.beforePathsWithMatches
            , currentMatchPath = currentMatchPath
            }

        unifiedConfigAfter =
            { onToggleExpand = AfterToggleExpand
            , searchMatches = model.searchResult.afterPathsWithMatches
            , currentMatchPath = currentMatchPath
            }
    in
    div [ class "flex flex-col h-full gap-4" ]
        [ div [ class "grid grid-cols-2 gap-4 flex-1 min-h-0" ]
            [ -- Before (left) panel
              div [ class "bg-base-100 rounded-lg border border-base-300 flex flex-col overflow-hidden" ]
                [ div [ class "flex items-center justify-between px-4 py-3 border-b border-base-300 bg-base-200/50" ]
                    [ h3 [ class "font-semibold text-base" ] [ text "Model Before Message" ]
                    , div [ class "flex items-center gap-2" ]
                        [ if showFiltered then
                            span [ class "badge badge-info badge-sm" ]
                                [ text (String.fromInt matchCount ++ " matches") ]

                          else
                            text ""
                        , if showFiltered then
                            viewCollapseExpandButtons CollapseAllFilter ExpandAllFilter

                          else
                            viewCollapseExpandButtons CollapseAllBefore ExpandAllBefore
                        ]
                    ]
                , div [ class "flex-1 overflow-auto p-4" ]
                    [ if isFirstMessage then
                        viewInitialStateMessage

                      else if showFiltered then
                        case maybeBeforeState of
                            Just beforeJson ->
                                TreeView.viewFiltered filterConfigBefore beforeJson model.filterExpandedPaths

                            Nothing ->
                                TreeView.viewEmpty

                      else if showDiffHighlighting then
                        -- Show diff highlighting on the before panel (removed items highlighted)
                        case maybeBeforeState of
                            Just beforeJson ->
                                TreeView.viewDiffBefore beforeDiffConfig beforeJson viewState.beforeExpandedPaths

                            Nothing ->
                                TreeView.viewEmpty

                      else
                        case maybeBeforeState of
                            Just beforeJson ->
                                TreeView.viewUnified unifiedConfigBefore beforeJson viewState.beforeExpandedPaths

                            Nothing ->
                                TreeView.viewEmpty
                    ]
                ]

            -- After (right) panel
            , div [ class "bg-base-100 rounded-lg border border-base-300 flex flex-col overflow-hidden" ]
                [ div [ class "flex items-center justify-between px-4 py-3 border-b border-base-300 bg-base-200/50" ]
                    [ h3 [ class "font-semibold text-base" ] [ text "Model After Message" ]
                    , div [ class "flex items-center gap-2" ]
                        [ if showFiltered then
                            span [ class "badge badge-info badge-sm" ]
                                [ text (String.fromInt matchCount ++ " matches") ]

                          else if showDiffHighlighting && changeCount > 0 then
                            span [ class "badge badge-warning badge-sm" ]
                                [ text (String.fromInt changeCount ++ " changes") ]

                          else if showDiffHighlighting then
                            span [ class "badge badge-success badge-sm" ]
                                [ text "No changes" ]

                          else
                            text ""
                        , if showFiltered then
                            viewCollapseExpandButtons CollapseAllFilter ExpandAllFilter

                          else
                            viewCollapseExpandButtons CollapseAllAfter ExpandAllAfter
                        ]
                    ]
                , div [ class "flex-1 overflow-auto p-4" ]
                    [ if showFiltered then
                        case maybeAfterState of
                            Just afterJson ->
                                TreeView.viewFiltered filterConfigAfter afterJson model.filterExpandedPaths

                            Nothing ->
                                TreeView.viewEmpty

                      else if showDiffHighlighting then
                        case maybeAfterState of
                            Just afterJson ->
                                TreeView.viewDiff diffConfig afterJson viewState.afterExpandedPaths

                            Nothing ->
                                TreeView.viewEmpty

                      else
                        case maybeAfterState of
                            Just afterJson ->
                                TreeView.viewUnified unifiedConfigAfter afterJson viewState.afterExpandedPaths

                            Nothing ->
                                TreeView.viewEmpty
                    ]
                ]
            ]

        -- Legend (only shown when diff highlighting is active and not filtering)
        , if showDiffHighlighting && not showFiltered then
            viewDiffLegend

          else
            text ""
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


{-| Render collapse/expand all buttons for tree panel headers.
-}
viewCollapseExpandButtons : Msg -> Msg -> Html Msg
viewCollapseExpandButtons collapseMsg expandMsg =
    div [ class "flex items-center gap-1" ]
        [ div [ class "tooltip tooltip-bottom", attribute "data-tip" "Collapse all" ]
            [ button
                [ class "btn btn-ghost btn-sm px-2 text-lg"
                , onClick collapseMsg
                ]
                [ text "⊟" ]
            ]
        , div [ class "tooltip tooltip-bottom", attribute "data-tip" "Expand all" ]
            [ button
                [ class "btn btn-ghost btn-sm px-2 text-lg"
                , onClick expandMsg
                ]
                [ text "⊞" ]
            ]
        ]


{-| Render the diff legend showing what each color means.
-}
viewDiffLegend : Html Msg
viewDiffLegend =
    div [ class "flex items-center gap-4 px-4 py-2 border-t border-base-300 bg-base-200/30 text-xs rounded-b-lg" ]
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


{-| Render the message details panel section.

Displays the selected message's type name and payload data. The payload
is rendered using the same tree renderer as effects and state views.

This panel is positioned at the top of the main content area, below the
view options toolbar and above the effects panel.

-}
viewMessageDetailsPanel : Model -> Html Msg
viewMessageDetailsPanel model =
    case model.selectedIndex of
        Nothing ->
            text ""

        Just index ->
            let
                maybeEntry =
                    model.logEntries
                        |> List.drop index
                        |> List.head
            in
            case maybeEntry of
                Nothing ->
                    text ""

                Just entry ->
                    let
                        viewState =
                            getMessageViewState model

                        currentMatchPath =
                            getCurrentMatchPath model

                        config : TreeView.UnifiedConfig Msg
                        config =
                            { onToggleExpand = PayloadToggleExpand
                            , searchMatches = model.searchResult.payloadPathsWithMatches
                            , currentMatchPath = currentMatchPath
                            }

                        -- Highlight message name if it matches the search
                        messageNameHighlight =
                            if model.searchResult.messageNameMatches then
                                " search-match"

                            else
                                ""
                    in
                    div [ class "mx-4 mt-4" ]
                        [ div [ class "bg-base-200 rounded-lg border border-base-300 overflow-hidden" ]
                            [ div [ class "flex items-center justify-between px-3 py-2 border-b border-base-300" ]
                                [ div [ class "flex items-center gap-2" ]
                                    [ span [ class "text-sm font-medium text-base-content/60" ] [ text "Message" ]
                                    , span
                                        [ id "message-name"
                                        , class ("font-mono font-medium" ++ messageNameHighlight)
                                        ]
                                        [ text entry.message.name ]
                                    ]
                                , viewCollapseExpandButtons CollapseAllPayload ExpandAllPayload
                                ]
                            , div [ class "p-3" ]
                                [ TreeView.viewUnified config entry.message.payload viewState.payloadExpandedPaths ]
                            ]
                        ]


{-| Render the effects panel section.

Displays the list of effects (commands) produced by the selected message's
update function. Effects are shown in a collapsible accordion panel with
their names and associated data.

This panel is positioned above the state display so users can see both
the effects and state simultaneously when the panel is expanded.

-}
viewEffectsPanel : Model -> Html Msg
viewEffectsPanel model =
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

        -- Get the effect expanded paths from the current message's view state
        viewState =
            getMessageViewState model
    in
    div [ class "mx-4 mt-4" ]
        [ div [ class "collapse collapse-arrow bg-base-200 rounded-lg border border-base-300" ]
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
                [ viewEffectsList model viewState.effectExpandedPaths selectedEffects ]
            ]
        ]


{-| Render the list of effects or an empty state message.
-}
viewEffectsList : Model -> Dict Int (Set String) -> List Effect -> Html Msg
viewEffectsList model effectExpandedPaths effects =
    if List.isEmpty effects then
        p [ class "text-sm text-base-content/60 py-2" ]
            [ text "No effects for this message" ]

    else
        ul [ class "space-y-2 pt-2" ]
            (List.indexedMap (viewEffectItem model effectExpandedPaths) effects)


{-| Render a single effect item with its name and data using TreeView.
-}
viewEffectItem : Model -> Dict Int (Set String) -> Int -> Effect -> Html Msg
viewEffectItem model effectExpandedPaths index effect =
    let
        expandedPaths =
            Dict.get index effectExpandedPaths
                |> Maybe.withDefault (Set.singleton "")

        -- Get search matches for this effect's data
        effectDataMatches =
            Dict.get index model.searchResult.effectDataPathsWithMatches
                |> Maybe.withDefault Set.empty

        currentMatchPath =
            getCurrentMatchPath model

        config : TreeView.UnifiedConfig Msg
        config =
            { onToggleExpand = EffectToggleExpand index
            , searchMatches = effectDataMatches
            , currentMatchPath = currentMatchPath
            }

        -- Highlight effect name if it matches the search
        effectNameMatches =
            Set.member index model.searchResult.effectNameMatches

        effectNameHighlight =
            if effectNameMatches then
                " search-match"

            else
                ""
    in
    li [ class "bg-base-100 rounded-lg border border-base-300 overflow-hidden" ]
        [ div [ class "flex items-center justify-between px-3 py-2 bg-base-200/50 border-b border-base-300" ]
            [ div [ class "flex items-center gap-2" ]
                [ span [ class "badge badge-outline badge-sm" ]
                    [ text (String.fromInt (index + 1)) ]
                , span
                    [ id ("effect-" ++ String.fromInt index ++ "-name")
                    , class ("font-mono font-medium text-sm" ++ effectNameHighlight)
                    ]
                    [ text effect.name ]
                ]
            , viewCollapseExpandButtons (CollapseAllEffect index) (ExpandAllEffect index)
            ]
        , div [ class "p-3" ]
            [ TreeView.viewUnified config effect.data expandedPaths ]
        ]


{-| Convert a MatchLocation to an element ID for scrolling.

Each section uses a different prefix to create unique element IDs:
  - Message name: "message-name"
  - Message payload: "payload-tree-node-{path}"
  - Effect name: "effect-{index}-name"
  - Effect data: "effect-{index}-tree-node-{path}"
  - Model before: "before-tree-node-{path}"
  - Model after: "tree-node-{path}" (default, for backward compatibility)

-}
matchLocationToElementId : Search.MatchLocation -> String
matchLocationToElementId location =
    case location of
        Search.InMessageName ->
            "message-name"

        Search.InMessagePayload path ->
            "payload-" ++ TreeView.pathToElementId (Search.pathToString path)

        Search.InEffectName index ->
            "effect-" ++ String.fromInt index ++ "-name"

        Search.InEffectData index path ->
            "effect-" ++ String.fromInt index ++ "-" ++ TreeView.pathToElementId (Search.pathToString path)

        Search.InModelBefore path ->
            "before-" ++ TreeView.pathToElementId (Search.pathToString path)

        Search.InModelAfter path ->
            TreeView.pathToElementId (Search.pathToString path)



-- SUBSCRIPTIONS


{-| Application subscriptions.

Subscribes to:

  - Incoming port for JavaScript messages
  - Global keyboard events for shortcuts:
      - Cmd/Ctrl+F: Focus search input
      - ArrowUp/ArrowDown: Navigate message list

Note: We use preventDefaultOn to prevent browser's default behaviors.

-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.incoming GotPortMessage
        , Browser.Events.onKeyDown keyboardShortcutDecoder
        , if model.isResizingSidebar then
            Sub.batch
                [ Browser.Events.onMouseMove (D.map ResizeSidebar (D.field "clientX" D.int))
                , Browser.Events.onMouseUp (D.succeed StopSidebarResize)
                ]

          else
            Sub.none
        ]


{-| Decoder for global keyboard shortcuts.

Handles:

  - Cmd/Ctrl+F: Focus the search input
  - ArrowUp: Select previous message in list (when not in input)
  - ArrowDown: Select next message in list (when not in input)

-}
keyboardShortcutDecoder : D.Decoder Msg
keyboardShortcutDecoder =
    D.map4
        (\key metaKey ctrlKey tagName ->
            let
                -- Don't handle arrow keys when focused on input elements
                isInputElement =
                    tagName == "INPUT" || tagName == "TEXTAREA" || tagName == "SELECT"
            in
            -- Cmd/Ctrl+F: Focus search input
            if key == "f" && (metaKey || ctrlKey) then
                FocusSearch

            else if key == "ArrowDown" && not isInputElement then
                -- Navigate down the list (to older messages at higher indices)
                SelectNextMessage

            else if key == "ArrowUp" && not isInputElement then
                -- Navigate up the list (to newer messages at lower indices)
                SelectPreviousMessage

            else
                NoOp
        )
        (D.field "key" D.string)
        (D.field "metaKey" D.bool)
        (D.field "ctrlKey" D.bool)
        (D.at [ "target", "tagName" ] D.string)
