module Main exposing (main)

{-| Main entry point for the TeaForge Debugger application.

This module implements The Elm Architecture (TEA) with Model, Msg, init, update,
view, and subscriptions. It provides the application shell and coordinates
communication with JavaScript via ports.

-}

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Browser.Events
import CompressionDict exposing (Compression)
import Dict exposing (Dict)
import Diff
import Filter
    exposing
        ( ActiveFilter
        , EditingFilter(..)
        , Filter(..)
        , FilterCategory(..)
        , FilterStatus(..)
        )
import FilterSidebar
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import LogParser
import MessageList
import Ports
import Search
import Set exposing (Set)
import Task
import TreeView
import Types
    exposing
        ( DisplayOrder(..)
        , Effect
        , ErrorEntryData
        , InitEntryData
        , InputSource(..)
        , LogEntry(..)
        , SubscriptionChangeData
        , TreePath
        , UpdateEntryData
        , WebSocketStatus(..)
        , getMessageName
        , getTimestamp
        )



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
    , startedSubExpandedPaths : Set String
    , stoppedSubExpandedPaths : Set String
    }


{-| Create default view state for a message.
-}
defaultMessageViewState : MessageViewState
defaultMessageViewState =
    { beforeExpandedPaths = Set.singleton ""
    , afterExpandedPaths = Set.singleton ""
    , effectExpandedPaths = Dict.empty
    , payloadExpandedPaths = Set.singleton ""
    , startedSubExpandedPaths = Set.singleton ""
    , stoppedSubExpandedPaths = Set.singleton ""
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
    , startedSubExpandedPaths = Set.singleton ""
    , stoppedSubExpandedPaths = Set.singleton ""
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

This structure manages:

  - Log entries received from the input source (stored as Array for efficient appending)
  - Active input source (file being read or WebSocket connection)
  - Currently selected message index
  - View options (show previous state, highlight changes)
  - Search and filter state
  - UI state (sidebar width, WebSocket modal)
  - Tree view states for after and before states
  - Diff view state (expanded paths, computed changes)
  - Per-message view states for retaining expansion state

-}
type alias Model =
    { logEntries : Array LogEntry
    , inputSource : Maybe InputSource
    , lastModelAfter : D.Value
    , selectedIndex : Maybe Int
    , displayOrder : DisplayOrder
    , showPreviousState : Bool
    , showChangedValues : Bool
    , searchQuery : String
    , searchResult : Search.EntrySearchResult
    , currentMatchIndex : Int
    , sidebarWidth : Int
    , isResizingSidebar : Bool
    , errorMessage : Maybe String
    , treeViewState : TreeView.State
    , beforeTreeViewState : TreeView.State
    , changedPaths : List TreePath
    , changes : Dict String Diff.Change
    , messageViewStates : Dict Int MessageViewState
    , compression : Compression
    , showWsModal : Bool
    , wsUrlInput : String
    , recentWsUrls : List String

    -- Advanced filter state
    , activeFilters : List ActiveFilter
    , filterSidebarOpen : Bool
    , filtersGlobalEnabled : Bool
    , filterEditing : Maybe ( Maybe Int, EditingFilter )
    , filteredIndices : Set Int
    }


{-| Flags passed from JavaScript on initialization.
-}
type alias Flags =
    { sidebarWidth : Int
    , recentWsUrls : List String
    }


{-| Decode flags from JavaScript.
-}
flagsDecoder : D.Decoder Flags
flagsDecoder =
    D.map2 Flags
        (D.field "sidebarWidth" D.int)
        (D.oneOf
            [ D.field "recentWsUrls" (D.list D.string)
            , D.succeed []
            ]
        )


{-| Initialize the model with default values.

The application starts with no active input source.
Sidebar width and recent WebSocket URLs are loaded from localStorage via flags.

-}
init : E.Value -> ( Model, Cmd Msg )
init flagsValue =
    let
        ( initialSidebarWidth, initialRecentWsUrls ) =
            case D.decodeValue flagsDecoder flagsValue of
                Ok flags ->
                    ( clamp 200 600 flags.sidebarWidth, flags.recentWsUrls )

                Err _ ->
                    ( 320, [] )
    in
    ( { logEntries = Array.empty
      , inputSource = Nothing
      , lastModelAfter = E.null
      , selectedIndex = Nothing
      , displayOrder = ReverseChronological
      , showPreviousState = False
      , showChangedValues = True
      , searchQuery = ""
      , searchResult = Search.emptyEntrySearchResult
      , currentMatchIndex = 0
      , sidebarWidth = initialSidebarWidth
      , isResizingSidebar = False
      , errorMessage = Nothing
      , treeViewState = TreeView.init
      , beforeTreeViewState = TreeView.init
      , changedPaths = []
      , changes = Dict.empty
      , messageViewStates = Dict.empty
      , compression = CompressionDict.empty
      , showWsModal = False
      , wsUrlInput = ""
      , recentWsUrls = initialRecentWsUrls
      , activeFilters = []
      , filterSidebarOpen = False
      , filtersGlobalEnabled = True
      , filterEditing = Nothing
      , filteredIndices = Set.empty
      }
    , Cmd.none
    )



-- MSG


{-| Messages that can be dispatched in the application.

These are organized into categories:

  - File operations (opening input sources)
  - WebSocket operations
  - Streaming (receiving entries)
  - Navigation (selecting messages)
  - View mode changes
  - Search operations
  - Port responses

-}
type Msg
    = -- File Operations
      OpenFileDialog
    | OpenInput String
    | InputOpened { success : Bool, path : Maybe String, error : Maybe String }
    | DisconnectSource
      -- WebSocket Operations
    | OpenWsConnectionModal
    | CloseWsConnectionModal
    | SetWsUrlInput String
    | SelectRecentWsUrl String
    | ConnectWebSocket
    | DisconnectWebSocket
    | WsConnecting
    | WsConnected
    | WsDisconnected
    | WsConnectionFailed
    | WsConnectionLost
      -- Streaming
    | EntryReceived EntryPayload
    | InputError String
    | InputClosed
      -- Navigation
    | SelectMessage Int
    | SelectNextMessage
    | SelectPreviousMessage
      -- View Options
    | ShowPreviousState
    | HidePreviousState
    | ShowChangedValues
    | HideChangedValues
    | SetDisplayOrder DisplayOrder
      -- Search
    | SetSearchQuery String
    | SelectNextMatch
    | SelectPreviousMatch
    | FocusSearch
    | NoOp
      -- Advanced Filters
    | FilterSidebarMsg FilterSidebar.Msg
    | QuickAddFilter Filter
      -- Tree View (After state)
    | TreeViewMsg TreeView.Msg
      -- Tree View (Before state - for split view)
    | BeforeTreeViewMsg TreeView.Msg
      -- Diff View
    | SetDiffExpanded (List String) Bool
      -- Unified View (before/after without diff highlighting)
    | SetBeforeExpanded (List String) Bool
    | SetAfterExpanded (List String) Bool
      -- Collapse/Expand All
    | CollapseAllBefore
    | ExpandAllBefore
    | CollapseAllAfter
    | ExpandAllAfter
      -- Effect Tree View
    | SetEffectExpanded Int (List String) Bool
    | CollapseAllEffect Int
    | ExpandAllEffect Int
      -- Message Payload Tree View
    | SetPayloadExpanded (List String) Bool
    | CollapseAllPayload
    | ExpandAllPayload
      -- Subscription Tree View
    | SetStartedSubscriptionsExpanded (List String) Bool
    | SetStoppedSubscriptionsExpanded (List String) Bool
      -- Port Communication
    | GotPortMessage E.Value
      -- Error Handling
    | DismissError
      -- Sidebar Resize
    | StartSidebarResize
    | ResizeSidebar Int
    | StopSidebarResize


{-| Payload for a received entry from the input source.
-}
type alias EntryPayload =
    { lineNumber : Int
    , entry : Maybe D.Value
    , error : Maybe String
    , rawText : Maybe String
    }



-- UPDATE


{-| Update function handling all application messages.

Each message handler returns the new model and any commands to execute.

-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- File Operations
        OpenFileDialog ->
            ( model
            , Ports.openFileDialog
            )

        OpenInput path ->
            -- Clear existing entries and open new input source
            ( { model
                | logEntries = Array.empty
                , inputSource = Just (FileSource { path = path, label = path })
                , lastModelAfter = E.null
                , selectedIndex = Nothing
                , errorMessage = Nothing
                , messageViewStates = Dict.empty
                , changedPaths = []
                , changes = Dict.empty
                , compression = CompressionDict.empty
                , filteredIndices = Set.empty
              }
            , Cmd.batch
                [ Ports.openInput path
                , Ports.disconnectWebSocket
                ]
            )

        InputOpened result ->
            if result.success then
                ( model, Cmd.none )

            else
                ( { model
                    | errorMessage = result.error
                    , inputSource = Nothing
                  }
                , Cmd.none
                )

        DisconnectSource ->
            ( { model | inputSource = Nothing }
            , case model.inputSource of
                Just (FileSource _) ->
                    Ports.closeInput

                Just (WebSocketSource _) ->
                    Ports.disconnectWebSocket

                Nothing ->
                    Cmd.none
            )

        -- Streaming
        EntryReceived payload ->
            handleEntryReceived payload model

        InputError errorMsg ->
            ( { model | errorMessage = Just errorMsg }
            , Cmd.none
            )

        InputClosed ->
            -- Stream finished, select last entry if none selected
            let
                entryCount =
                    Array.length model.logEntries

                newSelectedIndex =
                    if model.selectedIndex == Nothing && entryCount > 0 then
                        Just (entryCount - 1)

                    else
                        model.selectedIndex
            in
            case newSelectedIndex of
                Just idx ->
                    if model.selectedIndex == Nothing then
                        -- Auto-select the last entry
                        update (SelectMessage idx) model

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        -- WebSocket Operations
        OpenWsConnectionModal ->
            ( { model
                | showWsModal = True
                , wsUrlInput =
                    if String.isEmpty model.wsUrlInput then
                        List.head model.recentWsUrls |> Maybe.withDefault "ws://localhost:8080"

                    else
                        model.wsUrlInput
              }
            , Cmd.none
            )

        CloseWsConnectionModal ->
            ( { model | showWsModal = False }
            , Cmd.none
            )

        SetWsUrlInput url ->
            ( { model | wsUrlInput = url }
            , Cmd.none
            )

        SelectRecentWsUrl url ->
            ( { model | wsUrlInput = url }
            , Cmd.none
            )

        ConnectWebSocket ->
            let
                url =
                    String.trim model.wsUrlInput
            in
            if String.isEmpty url then
                ( model, Cmd.none )

            else
                ( { model
                    | showWsModal = False
                    , logEntries = Array.empty
                    , inputSource = Just (WebSocketSource { url = url, status = Connecting })
                    , lastModelAfter = E.null
                    , selectedIndex = Nothing
                    , errorMessage = Nothing
                    , messageViewStates = Dict.empty
                    , changedPaths = []
                    , changes = Dict.empty
                    , compression = CompressionDict.empty
                    , filteredIndices = Set.empty
                  }
                , Ports.connectWebSocket url
                )

        DisconnectWebSocket ->
            ( { model
                | inputSource =
                    case model.inputSource of
                        Just (WebSocketSource ws) ->
                            Just (WebSocketSource { ws | status = Disconnected })

                        other ->
                            other
              }
            , Ports.disconnectWebSocket
            )

        WsConnecting ->
            ( { model
                | inputSource =
                    case model.inputSource of
                        Just (WebSocketSource ws) ->
                            Just (WebSocketSource { ws | status = Connecting })

                        other ->
                            other
              }
            , Cmd.none
            )

        WsConnected ->
            let
                -- Add URL to recent list (at the front, remove duplicates)
                newRecentUrls =
                    case model.inputSource of
                        Just (WebSocketSource ws) ->
                            ws.url
                                :: List.filter ((/=) ws.url) model.recentWsUrls
                                |> List.take 5

                        _ ->
                            model.recentWsUrls
            in
            ( { model
                | inputSource =
                    case model.inputSource of
                        Just (WebSocketSource ws) ->
                            Just (WebSocketSource { ws | status = Connected })

                        other ->
                            other
                , recentWsUrls = newRecentUrls
              }
            , Cmd.none
            )

        WsDisconnected ->
            ( { model
                | inputSource =
                    case model.inputSource of
                        Just (WebSocketSource ws) ->
                            Just (WebSocketSource { ws | status = Disconnected })

                        other ->
                            other
              }
            , Cmd.none
            )

        WsConnectionFailed ->
            let
                errorMsg =
                    "Could not connect to server. Check that the server is running."
            in
            ( { model
                | inputSource =
                    case model.inputSource of
                        Just (WebSocketSource ws) ->
                            Just (WebSocketSource { ws | status = ConnectionError errorMsg })

                        other ->
                            other
                , errorMessage = Just errorMsg
              }
            , Cmd.none
            )

        WsConnectionLost ->
            let
                errorMsg =
                    "Connection to server was lost."
            in
            ( { model
                | inputSource =
                    case model.inputSource of
                        Just (WebSocketSource ws) ->
                            Just (WebSocketSource { ws | status = ConnectionError errorMsg })

                        other ->
                            other
                , errorMessage = Just errorMsg
              }
            , Cmd.none
            )

        -- Navigation
        SelectMessage index ->
            let
                -- Get the selected log entry
                maybeEntry =
                    Array.get index model.logEntries
            in
            case maybeEntry of
                Nothing ->
                    ( model, Cmd.none )

                Just entry ->
                    selectEntry index entry model

        -- Keyboard Navigation
        SelectNextMessage ->
            let
                maxIndex =
                    Array.length model.logEntries - 1

                newIndex =
                    case ( model.displayOrder, model.selectedIndex ) of
                        ( ReverseChronological, Just idx ) ->
                            Basics.max (idx - 1) 0

                        ( ReverseChronological, Nothing ) ->
                            maxIndex

                        ( Chronological, Just idx ) ->
                            Basics.min (idx + 1) maxIndex

                        ( Chronological, Nothing ) ->
                            0
            in
            if Array.isEmpty model.logEntries then
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
                    Array.length model.logEntries - 1

                newIndex =
                    case ( model.displayOrder, model.selectedIndex ) of
                        ( ReverseChronological, Just idx ) ->
                            Basics.min (idx + 1) maxIndex

                        ( ReverseChronological, Nothing ) ->
                            0

                        ( Chronological, Just idx ) ->
                            Basics.max (idx - 1) 0

                        ( Chronological, Nothing ) ->
                            maxIndex
            in
            if Array.isEmpty model.logEntries then
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
        ShowPreviousState ->
            ( { model | showPreviousState = True }
            , Cmd.none
            )

        HidePreviousState ->
            ( { model | showPreviousState = False }
            , Cmd.none
            )

        ShowChangedValues ->
            ( { model | showChangedValues = True }
            , Cmd.none
            )

        HideChangedValues ->
            ( { model | showChangedValues = False }
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
                        |> Maybe.andThen (\idx -> Array.get idx model.logEntries)

                -- Perform search on the entire entry
                newSearchResult =
                    case maybeEntry of
                        Just entry ->
                            searchLogEntry query entry

                        Nothing ->
                            Search.emptyEntrySearchResult
            in
            ( { model
                | searchQuery = query
                , searchResult = newSearchResult
                , currentMatchIndex = 0
              }
            , Cmd.none
            )

        SelectNextMatch ->
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

        SelectPreviousMatch ->
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
        SetDiffExpanded path expanded ->
            let
                pathKey =
                    String.join "." path

                setAfterPath state =
                    { state
                        | afterExpandedPaths =
                            if expanded then
                                Set.insert pathKey state.afterExpandedPaths

                            else
                                Set.remove pathKey state.afterExpandedPaths
                    }
            in
            ( updateMessageViewState setAfterPath model
            , Cmd.none
            )

        -- Unified View (before panel)
        SetBeforeExpanded path expanded ->
            let
                pathKey =
                    String.join "." path

                setBeforePath state =
                    { state
                        | beforeExpandedPaths =
                            if expanded then
                                Set.insert pathKey state.beforeExpandedPaths

                            else
                                Set.remove pathKey state.beforeExpandedPaths
                    }
            in
            ( updateMessageViewState setBeforePath model
            , Cmd.none
            )

        -- Unified View (after panel when not highlighting)
        SetAfterExpanded path expanded ->
            let
                pathKey =
                    String.join "." path

                setAfterPath state =
                    { state
                        | afterExpandedPaths =
                            if expanded then
                                Set.insert pathKey state.afterExpandedPaths

                            else
                                Set.remove pathKey state.afterExpandedPaths
                    }
            in
            ( updateMessageViewState setAfterPath model
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
                        |> Maybe.andThen (\idx -> Array.get idx model.logEntries)
                        |> Maybe.andThen getModelBefore

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
                        |> Maybe.andThen (\idx -> Array.get idx model.logEntries)
                        |> Maybe.andThen getModelAfter

                allPaths =
                    maybeAfterState
                        |> Maybe.map collectAllPaths
                        |> Maybe.withDefault Set.empty
            in
            ( updateMessageViewState (\state -> { state | afterExpandedPaths = Set.insert "" allPaths }) model
            , Cmd.none
            )

        -- Effect Tree View
        SetEffectExpanded effectIndex path expanded ->
            let
                pathKey =
                    String.join "." path

                setEffectPath state =
                    let
                        currentPaths =
                            Dict.get effectIndex state.effectExpandedPaths
                                |> Maybe.withDefault (Set.singleton "")

                        newPaths =
                            if expanded then
                                Set.insert pathKey currentPaths

                            else
                                Set.remove pathKey currentPaths
                    in
                    { state | effectExpandedPaths = Dict.insert effectIndex newPaths state.effectExpandedPaths }
            in
            ( updateMessageViewState setEffectPath model
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
                        |> Maybe.andThen (\idx -> Array.get idx model.logEntries)
                        |> Maybe.andThen getEffects
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
        SetPayloadExpanded path expanded ->
            let
                pathKey =
                    String.join "." path

                setPayloadPath state =
                    let
                        newPaths =
                            if expanded then
                                Set.insert pathKey state.payloadExpandedPaths

                            else
                                Set.remove pathKey state.payloadExpandedPaths
                    in
                    { state | payloadExpandedPaths = newPaths }
            in
            ( updateMessageViewState setPayloadPath model
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
                        |> Maybe.andThen (\idx -> Array.get idx model.logEntries)
                        |> Maybe.andThen getMessagePayload

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

        -- Subscription Tree View
        SetStartedSubscriptionsExpanded path expanded ->
            let
                pathKey =
                    String.join "." path

                setPath state =
                    let
                        newPaths =
                            if expanded then
                                Set.insert pathKey state.startedSubExpandedPaths

                            else
                                Set.remove pathKey state.startedSubExpandedPaths
                    in
                    { state | startedSubExpandedPaths = newPaths }
            in
            ( updateMessageViewState setPath model
            , Cmd.none
            )

        SetStoppedSubscriptionsExpanded path expanded ->
            let
                pathKey =
                    String.join "." path

                setPath state =
                    let
                        newPaths =
                            if expanded then
                                Set.insert pathKey state.stoppedSubExpandedPaths

                            else
                                Set.remove pathKey state.stoppedSubExpandedPaths
                    in
                    { state | stoppedSubExpandedPaths = newPaths }
            in
            ( updateMessageViewState setPath model
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

        -- Advanced Filters
        FilterSidebarMsg sidebarMsg ->
            handleFilterSidebarMsg sidebarMsg model

        QuickAddFilter filter ->
            let
                newFilter =
                    { status = Enabled, filter = filter }

                newFilters =
                    model.activeFilters ++ [ newFilter ]

                newModel =
                    { model
                        | activeFilters = newFilters
                        , filterSidebarOpen = True
                    }
            in
            ( recomputeFilteredIndices newModel, Cmd.none )


{-| Handle filter sidebar messages.
-}
handleFilterSidebarMsg : FilterSidebar.Msg -> Model -> ( Model, Cmd Msg )
handleFilterSidebarMsg msg model =
    case msg of
        FilterSidebar.ToggleSidebar ->
            ( { model | filterSidebarOpen = not model.filterSidebarOpen }
            , Cmd.none
            )

        FilterSidebar.ToggleGlobalFilters ->
            let
                newModel =
                    { model | filtersGlobalEnabled = not model.filtersGlobalEnabled }
            in
            ( recomputeFilteredIndices newModel, Cmd.none )

        FilterSidebar.ToggleCategoryFilters category ->
            let
                -- Check if all filters in this category are enabled
                categoryIsAllEnabled =
                    List.all
                        (\af ->
                            if Filter.filterCategory af.filter == category then
                                af.status == Enabled

                            else
                                True
                        )
                        model.activeFilters

                newStatus =
                    if categoryIsAllEnabled then
                        Disabled

                    else
                        Enabled

                newFilters =
                    List.map
                        (\af ->
                            if Filter.filterCategory af.filter == category then
                                { af | status = newStatus }

                            else
                                af
                        )
                        model.activeFilters

                newModel =
                    { model | activeFilters = newFilters }
            in
            ( recomputeFilteredIndices newModel, Cmd.none )

        FilterSidebar.ToggleFilter index ->
            let
                newFilters =
                    List.indexedMap
                        (\idx af ->
                            if idx == index then
                                { af
                                    | status =
                                        case af.status of
                                            Enabled ->
                                                Disabled

                                            Disabled ->
                                                Enabled
                                }

                            else
                                af
                        )
                        model.activeFilters

                newModel =
                    { model | activeFilters = newFilters }
            in
            ( recomputeFilteredIndices newModel, Cmd.none )

        FilterSidebar.DeleteFilter index ->
            let
                newFilters =
                    List.indexedMap Tuple.pair model.activeFilters
                        |> List.filterMap
                            (\( idx, af ) ->
                                if idx == index then
                                    Nothing

                                else
                                    Just af
                            )

                newModel =
                    { model | activeFilters = newFilters, filterEditing = Nothing }
            in
            ( recomputeFilteredIndices newModel, Cmd.none )

        FilterSidebar.StartAddFilter category ->
            ( { model | filterEditing = Just ( Nothing, Filter.emptyEditingFilter category ) }
            , Cmd.none
            )

        FilterSidebar.StartEditFilter index ->
            case List.head (List.drop index model.activeFilters) of
                Just af ->
                    ( { model | filterEditing = Just ( Just index, Filter.editingFilterFromActive af ) }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        FilterSidebar.CancelEdit ->
            ( { model | filterEditing = Nothing }
            , Cmd.none
            )

        FilterSidebar.SaveEdit ->
            case model.filterEditing of
                Just ( maybeIdx, editing ) ->
                    case Filter.editingFilterToFilter editing of
                        Just newFilter ->
                            let
                                newFilters =
                                    case maybeIdx of
                                        Nothing ->
                                            -- Adding new filter
                                            model.activeFilters ++ [ { status = Enabled, filter = newFilter } ]

                                        Just idx ->
                                            -- Editing existing filter
                                            List.indexedMap
                                                (\i af ->
                                                    if i == idx then
                                                        { af | filter = newFilter }

                                                    else
                                                        af
                                                )
                                                model.activeFilters

                                newModel =
                                    { model
                                        | activeFilters = newFilters
                                        , filterEditing = Nothing
                                    }
                            in
                            ( recomputeFilteredIndices newModel, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        FilterSidebar.UpdateEditingFilter newEditing ->
            ( { model | filterEditing = Maybe.map (\( idx, _ ) -> ( idx, newEditing )) model.filterEditing }
            , Cmd.none
            )

        FilterSidebar.ChangeEditingType newEditing ->
            ( { model | filterEditing = Maybe.map (\( idx, _ ) -> ( idx, newEditing )) model.filterEditing }
            , Cmd.none
            )


{-| Recompute the set of filtered indices based on current filters.
-}
recomputeFilteredIndices : Model -> Model
recomputeFilteredIndices model =
    let
        enabled =
            if model.filtersGlobalEnabled then
                Filter.enabledFilters model.activeFilters

            else
                []

        indices =
            if List.isEmpty enabled then
                -- No filters active, all entries visible
                Set.fromList (List.range 0 (Array.length model.logEntries - 1))

            else
                Array.toIndexedList model.logEntries
                    |> List.filterMap
                        (\( idx, entry ) ->
                            if Filter.matchesEntry enabled entry then
                                Just idx

                            else
                                Nothing
                        )
                    |> Set.fromList

        -- Clear selection if selected entry is no longer visible
        newSelectedIndex =
            case model.selectedIndex of
                Just idx ->
                    if Set.member idx indices || List.isEmpty enabled then
                        model.selectedIndex

                    else
                        Nothing

                Nothing ->
                    Nothing
    in
    { model
        | filteredIndices = indices
        , selectedIndex = newSelectedIndex
    }


{-| Handle a received entry from the input source.
-}
handleEntryReceived : EntryPayload -> Model -> ( Model, Cmd Msg )
handleEntryReceived payload model =
    case payload.error of
        Just errorMsg ->
            -- Malformed entry - add as ErrorEntry
            let
                errorEntry =
                    ErrorEntry
                        { lineNumber = payload.lineNumber
                        , rawText = Maybe.withDefault "" payload.rawText
                        , error = errorMsg
                        }
            in
            addEntryAndMaybeSelect errorEntry model

        Nothing ->
            case payload.entry of
                Just rawValue ->
                    processValidEntry payload.lineNumber rawValue model

                Nothing ->
                    ( model, Cmd.none )


{-| Process a valid JSON entry and add it to the log.
-}
processValidEntry : Int -> D.Value -> Model -> ( Model, Cmd Msg )
processValidEntry lineNum rawValue model =
    case D.decodeValue LogParser.entryTypeDecoder rawValue of
        Ok "header" ->
            -- Handle header entries to detect compression
            case D.decodeValue LogParser.headerDecoder rawValue of
                Ok headerData ->
                    if headerData.compression == Just "stringDict" then
                        ( { model | compression = CompressionDict.Enabled Dict.empty }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        Ok "stringDict" ->
            -- Merge string dictionary entries into compression state
            ( { model | compression = CompressionDict.merge rawValue model.compression }
            , Cmd.none
            )

        Ok "init" ->
            case D.decodeValue (LogParser.initDataDecoder model.compression) rawValue of
                Ok initData ->
                    let
                        entry =
                            InitEntry
                                { timestamp = initData.timestamp
                                , model = initData.model
                                , effects = initData.effects
                                }
                    in
                    addEntryAndMaybeSelect entry
                        { model | lastModelAfter = initData.model }

                Err e ->
                    addErrorEntry lineNum (D.errorToString e) model

        Ok "update" ->
            case D.decodeValue (LogParser.updateDataDecoder model.compression) rawValue of
                Ok updateData ->
                    let
                        entry =
                            UpdateEntry
                                { timestamp = updateData.timestamp
                                , message = updateData.message
                                , modelBefore = model.lastModelAfter
                                , modelAfter = updateData.model
                                , effects = updateData.effects
                                }
                    in
                    addEntryAndMaybeSelect entry
                        { model | lastModelAfter = updateData.model }

                Err e ->
                    addErrorEntry lineNum (D.errorToString e) model

        Ok "subscriptionChange" ->
            case D.decodeValue (LogParser.subscriptionChangeDataDecoder model.compression) rawValue of
                Ok subData ->
                    let
                        entry =
                            SubscriptionChangeEntry
                                { timestamp = subData.timestamp
                                , started = subData.started
                                , stopped = subData.stopped
                                }
                    in
                    addEntryAndMaybeSelect entry model

                Err e ->
                    addErrorEntry lineNum (D.errorToString e) model

        Ok unknownType ->
            -- Skip unknown entry types silently
            ( model, Cmd.none )

        Err e ->
            addErrorEntry lineNum (D.errorToString e) model


{-| Add an error entry for a malformed line.
-}
addErrorEntry : Int -> String -> Model -> ( Model, Cmd Msg )
addErrorEntry lineNum errorMsg model =
    let
        errorEntry =
            ErrorEntry
                { lineNumber = lineNum
                , rawText = ""
                , error = errorMsg
                }
    in
    addEntryAndMaybeSelect errorEntry model


{-| Add an entry to the log and auto-select if this is the first entry.
-}
addEntryAndMaybeSelect : LogEntry -> Model -> ( Model, Cmd Msg )
addEntryAndMaybeSelect entry model =
    let
        newEntries =
            Array.push entry model.logEntries

        -- Auto-select first entry if nothing is selected
        shouldAutoSelect =
            model.selectedIndex == Nothing && Array.isEmpty model.logEntries

        newModel =
            recomputeFilteredIndices { model | logEntries = newEntries }
    in
    if shouldAutoSelect then
        update (SelectMessage 0) newModel

    else
        ( newModel, Cmd.none )


{-| Select an entry and update the model accordingly.
-}
selectEntry : Int -> LogEntry -> Model -> ( Model, Cmd Msg )
selectEntry index entry model =
    let
        -- Get model states for tree view and diff
        ( maybeModelBefore, maybeModelAfter ) =
            ( getModelBefore entry, getModelAfter entry )

        -- Parse the after state for tree view
        newTreeState =
            maybeModelAfter
                |> Maybe.map
                    (\afterModel ->
                        case TreeView.parseValue afterModel model.treeViewState of
                            Ok state ->
                                state

                            Err _ ->
                                TreeView.init
                    )
                |> Maybe.withDefault TreeView.init

        -- Parse the before state for split view
        newBeforeTreeState =
            maybeModelBefore
                |> Maybe.map
                    (\beforeModel ->
                        case TreeView.parseValue beforeModel model.beforeTreeViewState of
                            Ok state ->
                                state

                            Err _ ->
                                TreeView.init
                    )
                |> Maybe.withDefault TreeView.init

        -- Compute diff
        diffResult =
            case ( maybeModelBefore, maybeModelAfter ) of
                ( Just before, Just after ) ->
                    Just (Diff.findChangedPaths before after)

                _ ->
                    Nothing

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
                searchLogEntry model.searchQuery entry

        -- Get or create the view state for this message
        existingViewState =
            Dict.get index model.messageViewStates

        messageViewState =
            case existingViewState of
                Just state ->
                    state

                Nothing ->
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
                Basics.min model.currentMatchIndex (newSearchResult.totalMatchCount - 1)
                    |> Basics.max 0
      }
    , Cmd.none
    )


{-| Handle incoming messages from JavaScript via ports.
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
                "openInput" ->
                    handleOpenInputRequest value model

                "inputOpened" ->
                    handleInputOpened value model

                "entryReceived" ->
                    handleEntryReceivedPort value model

                "inputError" ->
                    handleInputErrorPort value model

                "inputClosed" ->
                    update InputClosed model

                "wsConnecting" ->
                    update WsConnecting model

                "wsConnected" ->
                    update WsConnected model

                "wsDisconnected" ->
                    update WsDisconnected model

                "wsConnectionFailed" ->
                    update WsConnectionFailed model

                "wsConnectionLost" ->
                    update WsConnectionLost model

                "recentWsUrls" ->
                    handleRecentWsUrls value model

                _ ->
                    ( model, Cmd.none )

        Err _ ->
            ( { model | errorMessage = Just "Failed to decode port message" }
            , Cmd.none
            )


handleOpenInputRequest : E.Value -> Model -> ( Model, Cmd Msg )
handleOpenInputRequest value model =
    let
        decoder =
            D.field "payload" (D.field "path" D.string)
    in
    case D.decodeValue decoder value of
        Ok path ->
            update (OpenInput path) model

        Err _ ->
            ( { model | errorMessage = Just "Failed to decode open input request" }
            , Cmd.none
            )


handleInputOpened : E.Value -> Model -> ( Model, Cmd Msg )
handleInputOpened value model =
    let
        decoder =
            D.field "payload"
                (D.map3
                    (\success path error ->
                        { success = success
                        , path = path
                        , error = error
                        }
                    )
                    (D.field "success" D.bool)
                    (D.maybe (D.field "path" D.string))
                    (D.maybe (D.field "error" D.string))
                )
    in
    case D.decodeValue decoder value of
        Ok result ->
            update (InputOpened result) model

        Err _ ->
            ( { model | errorMessage = Just "Failed to decode input opened result" }
            , Cmd.none
            )


handleEntryReceivedPort : E.Value -> Model -> ( Model, Cmd Msg )
handleEntryReceivedPort value model =
    let
        decoder =
            D.field "payload"
                (D.map4
                    (\lineNumber entry error rawText ->
                        { lineNumber = lineNumber
                        , entry = entry
                        , error = error
                        , rawText = rawText
                        }
                    )
                    (D.field "lineNumber" D.int)
                    (D.maybe (D.field "entry" D.value))
                    (D.maybe (D.field "error" D.string))
                    (D.maybe (D.field "rawText" D.string))
                )
    in
    case D.decodeValue decoder value of
        Ok payload ->
            update (EntryReceived payload) model

        Err _ ->
            ( model, Cmd.none )


handleInputErrorPort : E.Value -> Model -> ( Model, Cmd Msg )
handleInputErrorPort value model =
    let
        decoder =
            D.field "payload" (D.field "error" D.string)
    in
    case D.decodeValue decoder value of
        Ok errorMsg ->
            update (InputError errorMsg) model

        Err _ ->
            ( { model | errorMessage = Just "An unknown error occurred" }
            , Cmd.none
            )


handleRecentWsUrls : E.Value -> Model -> ( Model, Cmd Msg )
handleRecentWsUrls value model =
    let
        decoder =
            D.field "payload" (D.list D.string)
    in
    case D.decodeValue decoder value of
        Ok urls ->
            ( { model | recentWsUrls = urls }, Cmd.none )

        Err _ ->
            ( model, Cmd.none )



-- HELPER FUNCTIONS FOR LOG ENTRIES


{-| Get the model before state from a log entry.
-}
getModelBefore : LogEntry -> Maybe D.Value
getModelBefore entry =
    case entry of
        InitEntry _ ->
            Nothing

        UpdateEntry data ->
            Just data.modelBefore

        SubscriptionChangeEntry _ ->
            Nothing

        ErrorEntry _ ->
            Nothing


{-| Get the model after state from a log entry.
-}
getModelAfter : LogEntry -> Maybe D.Value
getModelAfter entry =
    case entry of
        InitEntry data ->
            Just data.model

        UpdateEntry data ->
            Just data.modelAfter

        SubscriptionChangeEntry _ ->
            Nothing

        ErrorEntry _ ->
            Nothing


{-| Get the effects from a log entry.
-}
getEffects : LogEntry -> Maybe (List Effect)
getEffects entry =
    case entry of
        InitEntry data ->
            Just data.effects

        UpdateEntry data ->
            Just data.effects

        SubscriptionChangeEntry _ ->
            Nothing

        ErrorEntry _ ->
            Nothing


{-| Get the message payload from a log entry.
-}
getMessagePayload : LogEntry -> Maybe D.Value
getMessagePayload entry =
    case entry of
        UpdateEntry data ->
            Just data.message.payload

        _ ->
            Nothing


{-| Search within a log entry.
-}
searchLogEntry : String -> LogEntry -> Search.EntrySearchResult
searchLogEntry query entry =
    case entry of
        UpdateEntry data ->
            Search.searchEntry query
                { timestamp = data.timestamp
                , message = data.message
                , modelBefore = data.modelBefore
                , modelAfter = data.modelAfter
                , effects = data.effects
                }

        InitEntry data ->
            -- Search init entries similarly
            Search.searchEntry query
                { timestamp = data.timestamp
                , message = { name = "Init", payload = E.null }
                , modelBefore = E.null
                , modelAfter = data.model
                , effects = data.effects
                }

        _ ->
            Search.emptyEntrySearchResult


{-| Get the path of the current search match for highlighting in tree views.
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
    case D.decodeValue (D.keyValuePairs D.value) value of
        Ok pairs ->
            List.foldl
                (\( key, childValue ) acc ->
                    Set.union acc (collectAllPathsHelper (currentPath ++ [ key ]) childValue)
                )
                (Set.singleton pathKey)
                pairs

        Err _ ->
            case D.decodeValue (D.list D.value) value of
                Ok items ->
                    List.foldl
                        (\( idx, childValue ) acc ->
                            Set.union acc (collectAllPathsHelper (currentPath ++ [ String.fromInt idx ]) childValue)
                        )
                        (Set.singleton pathKey)
                        (List.indexedMap Tuple.pair items)

                Err _ ->
                    Set.empty



-- VIEW


{-| Main view function rendering the application layout.
-}
view : Model -> Html Msg
view model =
    div [ class "h-full w-full" ]
        [ div [ class "flex h-full w-full" ]
            [ div
                [ class "flex flex-col h-full overflow-hidden bg-base-200"
                , style "width" (String.fromInt model.sidebarWidth ++ "px")
                , style "flex-shrink" "0"
                ]
                [ viewSidebar model ]
            , viewResizeGutter model
            , div
                [ class "flex flex-col h-full overflow-hidden flex-1"
                ]
                [ viewMainContent model
                ]
            , if model.filterSidebarOpen then
                Html.map FilterSidebarMsg
                    (FilterSidebar.view
                        { filters = model.activeFilters
                        , isOpen = model.filterSidebarOpen
                        , globalEnabled = model.filtersGlobalEnabled
                        , editing = model.filterEditing
                        , totalEntries = Array.length model.logEntries
                        , visibleEntries = Set.size model.filteredIndices
                        }
                    )

              else
                text ""
            ]
        , viewErrorBanner model
        , viewWsModal model
        ]



-- BUTTON STYLES


{-| Primary button style for main action buttons (Open File, WebSocket, Connect).
Uses an outline style with border.
-}
primaryButtonClass : String
primaryButtonClass =
    "btn btn-outline"


{-| Secondary button style for Cancel and Disconnect buttons.
Uses a soft, less vibrant primary color.
-}
secondaryButtonClass : String
secondaryButtonClass =
    "btn btn-primary-soft"


{-| Render a dismissible error toast at the top of the screen.
-}
viewErrorBanner : Model -> Html Msg
viewErrorBanner model =
    case model.errorMessage of
        Just msg ->
            div
                [ class "fixed top-4 left-1/2 -translate-x-1/2 z-[10000]" ]
                [ div
                    [ class "alert alert-error flex items-center gap-2 px-4 py-3 rounded-lg shadow-lg bg-error text-error-content"
                    , attribute "role" "alert"
                    ]
                    [ i [ class "fa-solid fa-triangle-exclamation text-lg" ] []
                    , span [] [ text msg ]
                    , button
                        [ class "btn btn-sm btn-ghost"
                        , onClick DismissError
                        , title "Dismiss"
                        ]
                        [ text "✕" ]
                    ]
                ]

        Nothing ->
            text ""


{-| Render the WebSocket connection modal.
-}
viewWsModal : Model -> Html Msg
viewWsModal model =
    if model.showWsModal then
        div [ class "modal modal-open" ]
            [ div [ class "modal-box" ]
                [ h3 [ class "font-bold text-lg mb-4" ]
                    [ i [ class "fa-solid fa-plug mr-2" ] []
                    , text "WebSocket Connection"
                    ]
                , div [ class "form-control mb-4" ]
                    [ label [ class "label" ]
                        [ span [ class "label-text" ] [ text "WebSocket URL" ]
                        ]
                    , input
                        [ id "ws-url-input"
                        , type_ "text"
                        , class "input input-bordered w-full"
                        , placeholder "ws://localhost:8080"
                        , value model.wsUrlInput
                        , onInput SetWsUrlInput
                        , onEnterKey ConnectWebSocket
                        ]
                        []
                    ]
                , if List.isEmpty model.recentWsUrls then
                    text ""

                  else
                    div [ class "form-control mb-4" ]
                        [ label [ class "label" ]
                            [ span [ class "label-text" ] [ text "Recent URLs" ]
                            ]
                        , div [ class "flex flex-wrap gap-2" ]
                            (List.indexedMap
                                (\idx url ->
                                    button
                                        [ id ("btn-recent-url-" ++ String.fromInt idx)
                                        , class "btn btn-xs btn-outline"
                                        , onClick (SelectRecentWsUrl url)
                                        ]
                                        [ text url ]
                                )
                                model.recentWsUrls
                            )
                        ]
                , div [ class "modal-action" ]
                    [ button
                        [ id "btn-ws-cancel"
                        , class secondaryButtonClass
                        , onClick CloseWsConnectionModal
                        ]
                        [ text "Cancel" ]
                    , button
                        [ id "btn-ws-connect"
                        , class primaryButtonClass
                        , type_ "button"
                        , onClick ConnectWebSocket
                        , disabled (String.isEmpty (String.trim model.wsUrlInput))
                        ]
                        [ text "Connect" ]
                    ]
                ]
            ]

    else
        text ""


{-| Handle Enter key press on an input.
-}
onEnterKey : Msg -> Attribute Msg
onEnterKey msg =
    Html.Events.on "keydown"
        (D.field "key" D.string
            |> D.andThen
                (\key ->
                    if key == "Enter" then
                        D.succeed msg

                    else
                        D.fail "not enter"
                )
        )


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


onMouseDown : msg -> Attribute msg
onMouseDown msg =
    Html.Events.preventDefaultOn "mousedown"
        (D.succeed ( msg, True ))


{-| Render the sidebar with message list.
-}
viewSidebar : Model -> Html Msg
viewSidebar model =
    let
        hasActiveFilters =
            model.filtersGlobalEnabled && Filter.enabledFilterCount model.activeFilters > 0

        totalCount =
            Array.length model.logEntries

        visibleCount =
            if hasActiveFilters then
                Set.size model.filteredIndices

            else
                totalCount

        messageCountText =
            if hasActiveFilters then
                String.fromInt visibleCount ++ " / " ++ String.fromInt totalCount ++ " messages"

            else
                String.fromInt totalCount ++ " messages"
    in
    div [ class "flex flex-col h-full overflow-hidden min-w-0" ]
        [ viewInputSourceHeader model
        , div [ class "p-4 border-b border-base-300 shrink-0" ]
            [ div [ class "flex items-center justify-between" ]
                [ h2 [ class "font-semibold text-lg" ] [ text "Messages" ]
                , Html.map FilterSidebarMsg
                    (FilterSidebar.viewToggleButton
                        { filters = model.activeFilters
                        , isOpen = model.filterSidebarOpen
                        , globalEnabled = model.filtersGlobalEnabled
                        , editing = model.filterEditing
                        , totalEntries = totalCount
                        , visibleEntries = visibleCount
                        }
                    )
                ]
            , div [ class "flex flex-nowrap items-center gap-2 mt-1" ]
                [ span [ class "text-sm text-base-content/60 whitespace-nowrap flex-none" ]
                    [ text messageCountText ]
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
                ]
            ]
        , div [ class "flex-1 overflow-y-auto overflow-x-hidden min-w-0" ]
            [ MessageList.view
                { selectedIndex = model.selectedIndex
                , onSelect = SelectMessage
                , entries = model.logEntries
                , displayOrder = model.displayOrder
                , visibleIndices = model.filteredIndices
                , isFiltered = hasActiveFilters
                }
            ]
        ]


{-| Render the input source header with status and buttons.
-}
viewInputSourceHeader : Model -> Html Msg
viewInputSourceHeader model =
    div [ class "p-3 border-b border-base-300 shrink-0 bg-base-100" ]
        [ case model.inputSource of
            Nothing ->
                div [ class "flex flex-col gap-2" ]
                    [ div [ class "text-sm text-base-content/60" ] [ text "No message source selected" ]
                    , div [ class "flex gap-2" ]
                        [ button
                            [ id "btn-open-file"
                            , class (primaryButtonClass ++ " btn-sm flex-1")
                            , onClick OpenFileDialog
                            ]
                            [ i [ class "fa-solid fa-folder-open mr-1" ] []
                            , text "Open File"
                            ]
                        , button
                            [ id "btn-websocket"
                            , class (primaryButtonClass ++ " btn-sm flex-1")
                            , onClick OpenWsConnectionModal
                            ]
                            [ i [ class "fa-solid fa-plug mr-1" ] []
                            , text "WebSocket"
                            ]
                        ]
                    ]

            Just (FileSource fileData) ->
                div [ class "flex flex-col gap-2" ]
                    [ div [ class "flex items-center gap-2" ]
                        [ span [ class "text-xs text-base-content/60 font-medium" ] [ text "Message Source" ]
                        , span [ class "text-sm truncate flex-1", title fileData.path ]
                            [ text (truncatePath fileData.path) ]
                        ]
                    , button
                        [ id "btn-disconnect"
                        , class (secondaryButtonClass ++ " btn-xs w-full")
                        , onClick DisconnectSource
                        ]
                        [ text "Disconnect" ]
                    ]

            Just (WebSocketSource wsData) ->
                case wsData.status of
                    Connected ->
                        div [ class "flex flex-col gap-2" ]
                            [ div [ class "flex items-center gap-2" ]
                                [ viewWsStatusIndicator wsData.status
                                , span [ class "text-xs text-base-content/60 font-medium" ] [ text "Message Source" ]
                                , span [ class "text-sm truncate flex-1", title wsData.url ]
                                    [ text wsData.url ]
                                ]
                            , button
                                [ id "btn-disconnect"
                                , class (secondaryButtonClass ++ " btn-xs w-full")
                                , onClick DisconnectSource
                                ]
                                [ text "Disconnect" ]
                            ]

                    Connecting ->
                        div [ class "flex flex-col gap-2" ]
                            [ div [ class "flex items-center gap-2" ]
                                [ viewWsStatusIndicator wsData.status
                                , span [ class "text-xs text-base-content/60 font-medium" ] [ text "Message Source" ]
                                , span [ class "text-sm truncate flex-1", title wsData.url ]
                                    [ text wsData.url ]
                                ]
                            , button
                                [ id "btn-connecting"
                                , class (secondaryButtonClass ++ " btn-xs w-full")
                                , disabled True
                                ]
                                [ span [ class "loading loading-spinner loading-xs" ] []
                                , text "Connecting..."
                                ]
                            ]

                    _ ->
                        -- Disconnected or Error - show status and allow reconnection
                        div [ class "flex flex-col gap-2" ]
                            [ div [ class "flex items-center gap-2" ]
                                [ viewWsStatusIndicator wsData.status
                                , span [ class "text-xs text-base-content/60 font-medium" ] [ text "Message Source" ]
                                , span [ class "text-sm truncate flex-1", title wsData.url ]
                                    [ text wsData.url ]
                                ]
                            , div [ class "flex gap-2" ]
                                [ button
                                    [ id "btn-open-file"
                                    , class (primaryButtonClass ++ " btn-sm flex-1")
                                    , onClick OpenFileDialog
                                    ]
                                    [ i [ class "fa-solid fa-folder-open mr-1" ] []
                                    , text "Open File"
                                    ]
                                , button
                                    [ id "btn-websocket"
                                    , class (primaryButtonClass ++ " btn-sm flex-1")
                                    , onClick OpenWsConnectionModal
                                    ]
                                    [ i [ class "fa-solid fa-plug mr-1" ] []
                                    , text "WebSocket"
                                    ]
                                ]
                            ]
        ]


{-| Render WebSocket status indicator.
-}
viewWsStatusIndicator : WebSocketStatus -> Html Msg
viewWsStatusIndicator status =
    case status of
        Connected ->
            span [ class "flex items-center gap-1" ]
                [ span [ class "ws-status-indicator ws-status-connected" ] []
                , span [ class "text-xs text-success font-medium" ] [ text "Live" ]
                ]

        Connecting ->
            span [ class "flex items-center gap-1" ]
                [ span [ class "loading loading-spinner loading-xs" ] []
                , span [ class "text-xs text-base-content/60" ] [ text "Connecting" ]
                ]

        Disconnected ->
            span [ class "flex items-center gap-1" ]
                [ span [ class "ws-status-indicator ws-status-disconnected" ] []
                , span [ class "text-xs text-error" ] [ text "Disconnected" ]
                ]

        ConnectionError _ ->
            span [ class "flex items-center gap-1" ]
                [ span [ class "ws-status-indicator ws-status-error" ] []
                , span [ class "text-xs text-error" ] [ text "Disconnected" ]
                ]


{-| Truncate a file path for display.
-}
truncatePath : String -> String
truncatePath path =
    let
        parts =
            String.split "/" path

        filename =
            List.reverse parts |> List.head |> Maybe.withDefault path
    in
    if String.length path > 30 then
        ".../" ++ filename

    else
        path


{-| Render the main content area.
-}
viewMainContent : Model -> Html Msg
viewMainContent model =
    case model.selectedIndex of
        Nothing ->
            -- No message selected - show placeholder
            main_ [ class "flex-1 flex flex-col overflow-hidden" ]
                [ div [ class "flex-1 overflow-auto p-4" ]
                    [ viewNoSelection model ]
                ]

        Just index ->
            case Array.get index model.logEntries of
                Nothing ->
                    -- Invalid index - show placeholder
                    main_ [ class "flex-1 flex flex-col overflow-hidden" ]
                        [ div [ class "flex-1 overflow-auto p-4" ]
                            [ viewNoSelection model ]
                        ]

                Just entry ->
                    -- Message selected - show full state view
                    main_ [ class "flex-1 flex flex-col overflow-hidden" ]
                        [ div [ class "p-4 border-b border-base-300 shrink-0" ]
                            [ h2 [ class "font-semibold text-lg" ] [ text "Model" ]
                            ]
                        , viewViewOptions model
                        , div [ class "flex-1 overflow-auto" ]
                            [ viewMessageDetailsPanel model
                            , viewEffectsPanel model
                            , div [ class "mx-4 mt-4 mb-4" ]
                                [ viewSelectedEntry model index entry ]
                            ]
                        ]


{-| Render the view options bar with checkboxes and search.
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
                        , onClick
                            (if model.showPreviousState then
                                HidePreviousState

                             else
                                ShowPreviousState
                            )
                        ]
                        []
                    , span [ class "label-text text-sm" ] [ text "Show previous state" ]
                    ]
                , label [ class "label cursor-pointer gap-2" ]
                    [ input
                        [ type_ "checkbox"
                        , class "checkbox checkbox-sm"
                        , checked model.showChangedValues
                        , onClick
                            (if model.showChangedValues then
                                HideChangedValues

                             else
                                ShowChangedValues
                            )
                        ]
                        []
                    , span [ class "label-text text-sm" ] [ text "Highlight changes" ]
                    ]
                ]
            , viewSearchBox model
            ]
        ]


{-| Render the search box with navigation controls.
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
        [ input
            [ type_ "text"
            , id "search-input"
            , placeholder "Search... (\u{2318}F)"
            , class "input input-sm w-40"
            , value model.searchQuery
            , onInput SetSearchQuery
            , preventDefaultOn "keydown" searchKeyDecoder
            ]
            []
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
        , div [ class "flex items-center" ]
            [ div [ class "tooltip tooltip-bottom", attribute "data-tip" "Previous match (Shift+Enter)" ]
                [ button
                    [ class "btn btn-ghost btn-xs px-1"
                    , onClick SelectPreviousMatch
                    , disabled (not hasMatches)
                    ]
                    [ text "\u{25B2}" ]
                ]
            , div [ class "tooltip tooltip-bottom", attribute "data-tip" "Next match (Enter)" ]
                [ button
                    [ class "btn btn-ghost btn-xs px-1"
                    , onClick SelectNextMatch
                    , disabled (not hasMatches)
                    ]
                    [ text "\u{25BC}" ]
                ]
            ]
        ]


searchKeyDecoder : D.Decoder ( Msg, Bool )
searchKeyDecoder =
    D.map2
        (\key shiftKey ->
            if key == "Enter" then
                if shiftKey then
                    ( SelectPreviousMatch, True )

                else
                    ( SelectNextMatch, True )

            else
                ( NoOp, False )
        )
        (D.field "key" D.string)
        (D.field "shiftKey" D.bool)


{-| Render placeholder when no message is selected.
-}
viewNoSelection : Model -> Html Msg
viewNoSelection model =
    div [ class "h-full flex items-center justify-center" ]
        [ div [ class "text-center text-base-content/60" ]
            (if Array.isEmpty model.logEntries then
                [ div [ class "text-5xl mb-4" ] [ i [ class "fa-solid fa-comments" ] [] ]
                , p [ class "text-lg font-medium" ] [ text "No Messages Loaded" ]
                , p [ class "text-sm mt-2" ] [ text "Open a file or connect to a WebSocket to begin" ]
                ]

             else
                [ div [ class "text-5xl mb-4" ] [ text "👆" ]
                , p [ class "text-lg font-medium" ] [ text "No message selected" ]
                , p [ class "text-sm mt-2" ] [ text "Select a message from the sidebar to view its state" ]
                ]
            )
        ]


{-| Render the selected entry based on its type.
-}
viewSelectedEntry : Model -> Int -> LogEntry -> Html Msg
viewSelectedEntry model index entry =
    case entry of
        ErrorEntry data ->
            viewErrorEntry data

        InitEntry data ->
            viewInitEntry model index data

        UpdateEntry data ->
            viewUpdateEntry model index data

        SubscriptionChangeEntry data ->
            viewSubscriptionEntry model data


{-| Render an error entry.
-}
viewErrorEntry : ErrorEntryData -> Html Msg
viewErrorEntry data =
    div [ class "alert alert-error" ]
        [ i [ class "fa-solid fa-triangle-exclamation text-xl" ] []
        , div []
            [ div [ class "font-bold" ] [ text ("Parse error on line " ++ String.fromInt data.lineNumber) ]
            , div [] [ text data.error ]
            , if not (String.isEmpty data.rawText) then
                div [ class "mt-2 font-mono text-sm opacity-70 truncate max-w-lg" ] [ text data.rawText ]

              else
                text ""
            ]
        ]


{-| Render an init entry.
-}
viewInitEntry : Model -> Int -> InitEntryData -> Html Msg
viewInitEntry model index data =
    let
        viewState =
            getMessageViewState model

        currentMatchPath =
            getCurrentMatchPath model
    in
    div [ class "bg-base-100 rounded-lg border border-base-300 flex flex-col h-full overflow-hidden" ]
        [ div [ class "flex items-center justify-between px-4 py-3 border-b border-base-300 bg-base-200/50" ]
            [ h3 [ class "font-semibold text-base" ] [ text "Initial State" ]
            , viewCollapseExpandButtons CollapseAllAfter ExpandAllAfter
            ]
        , div [ class "flex-1 overflow-auto p-4" ]
            [ TreeView.viewUnified
                { onToggleExpand = SetAfterExpanded
                , searchMatches = model.searchResult.afterPathsWithMatches
                , currentMatchPath = currentMatchPath
                , onQuickAddFilter = Just quickAddModelFilter
                }
                data.model
                viewState.afterExpandedPaths
            ]
        ]


{-| Render an update entry.
-}
viewUpdateEntry : Model -> Int -> UpdateEntryData -> Html Msg
viewUpdateEntry model index data =
    let
        isFirstMessage =
            index == 0
    in
    if model.showPreviousState then
        viewSplitMode model index data isFirstMessage

    else
        viewSingleStateMode model index data


{-| Render a subscription change entry.
-}
viewSubscriptionEntry : Model -> SubscriptionChangeData -> Html Msg
viewSubscriptionEntry model data =
    let
        viewState =
            getMessageViewState model

        startedConfig : TreeView.UnifiedConfig Msg
        startedConfig =
            { onToggleExpand = SetStartedSubscriptionsExpanded
            , searchMatches = Set.empty
            , currentMatchPath = Nothing
            , onQuickAddFilter = Nothing
            }

        stoppedConfig : TreeView.UnifiedConfig Msg
        stoppedConfig =
            { onToggleExpand = SetStoppedSubscriptionsExpanded
            , searchMatches = Set.empty
            , currentMatchPath = Nothing
            , onQuickAddFilter = Nothing
            }
    in
    div [ class "bg-base-100 rounded-lg border border-base-300 p-4" ]
        [ h3 [ class "font-semibold text-base mb-4" ] [ text "Subscription Change" ]
        , div [ class "grid grid-cols-2 gap-4" ]
            [ div []
                [ h4 [ class "font-medium text-success mb-2" ] [ text "Started" ]
                , if List.isEmpty data.started then
                    p [ class "text-sm text-base-content/60" ] [ text "None" ]

                  else
                    div [ class "text-sm" ]
                        [ TreeView.viewUnified startedConfig (E.list identity data.started) viewState.startedSubExpandedPaths ]
                ]
            , div []
                [ h4 [ class "font-medium text-error mb-2" ] [ text "Stopped" ]
                , if List.isEmpty data.stopped then
                    p [ class "text-sm text-base-content/60" ] [ text "None" ]

                  else
                    div [ class "text-sm" ]
                        [ TreeView.viewUnified stoppedConfig (E.list identity data.stopped) viewState.stoppedSubExpandedPaths ]
                ]
            ]
        ]


{-| Render the single state view mode.
-}
viewSingleStateMode : Model -> Int -> UpdateEntryData -> Html Msg
viewSingleStateMode model index data =
    let
        isFirstMessage =
            index == 0

        changeCount =
            List.length model.changedPaths

        viewState =
            getMessageViewState model

        currentMatchPath =
            getCurrentMatchPath model
    in
    if model.showChangedValues && not isFirstMessage then
        let
            treeViewChanges =
                Dict.map (\_ change -> diffChangeToTreeViewChange change) model.changes

            diffConfig =
                { changedPaths = model.changedPaths
                , changes = treeViewChanges
                , changesOnly = False
                , onToggleExpand = SetDiffExpanded
                , searchMatches = model.searchResult.afterPathsWithMatches
                , currentMatchPath = currentMatchPath
                , onQuickAddFilter = Just quickAddModelFilter
                }
        in
        div [ class "bg-base-100 rounded-lg border border-base-300 flex flex-col h-full overflow-hidden" ]
            [ div [ class "flex items-center justify-between px-4 py-3 border-b border-base-300 bg-base-200/50" ]
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
            , div [ class "flex-1 overflow-auto p-4" ]
                [ TreeView.viewDiff diffConfig data.modelAfter viewState.afterExpandedPaths ]
            , viewDiffLegend
            ]

    else
        div [ class "bg-base-100 rounded-lg border border-base-300 flex flex-col h-full overflow-hidden" ]
            [ div [ class "flex items-center justify-between px-4 py-3 border-b border-base-300 bg-base-200/50" ]
                [ h3 [ class "font-semibold text-base" ] [ text "Current State" ]
                , viewCollapseExpandButtons CollapseAllAfter ExpandAllAfter
                ]
            , div [ class "flex-1 overflow-auto p-4" ]
                [ TreeView.viewUnified
                    { onToggleExpand = SetAfterExpanded
                    , searchMatches = model.searchResult.afterPathsWithMatches
                    , currentMatchPath = currentMatchPath
                    , onQuickAddFilter = Just quickAddModelFilter
                    }
                    data.modelAfter
                    viewState.afterExpandedPaths
                ]
            ]


{-| Render the split view mode with before and after states side by side.
-}
viewSplitMode : Model -> Int -> UpdateEntryData -> Bool -> Html Msg
viewSplitMode model index data isFirstMessage =
    let
        changeCount =
            List.length model.changedPaths

        showDiffHighlighting =
            model.showChangedValues && not isFirstMessage

        viewState =
            getMessageViewState model

        treeViewChanges =
            Dict.map (\_ change -> diffChangeToTreeViewChange change) model.changes

        currentMatchPath =
            getCurrentMatchPath model

        diffConfig =
            { changedPaths = model.changedPaths
            , changes = treeViewChanges
            , changesOnly = False
            , onToggleExpand = SetDiffExpanded
            , searchMatches = model.searchResult.afterPathsWithMatches
            , currentMatchPath = currentMatchPath
            , onQuickAddFilter = Just quickAddModelFilter
            }

        beforeDiffConfig =
            { changedPaths = model.changedPaths
            , changes = treeViewChanges
            , onToggleExpand = SetBeforeExpanded
            , searchMatches = model.searchResult.beforePathsWithMatches
            , currentMatchPath = currentMatchPath
            , onQuickAddFilter = Just quickAddModelFilter
            }

        unifiedConfigBefore =
            { onToggleExpand = SetBeforeExpanded
            , searchMatches = model.searchResult.beforePathsWithMatches
            , currentMatchPath = currentMatchPath
            , onQuickAddFilter = Just quickAddModelFilter
            }

        unifiedConfigAfter =
            { onToggleExpand = SetAfterExpanded
            , searchMatches = model.searchResult.afterPathsWithMatches
            , currentMatchPath = currentMatchPath
            , onQuickAddFilter = Just quickAddModelFilter
            }
    in
    div [ class "flex flex-col h-full gap-4" ]
        [ div [ class "grid grid-cols-2 gap-4 flex-1 min-h-0" ]
            [ -- Before (left) panel
              div [ class "bg-base-100 rounded-lg border border-base-300 flex flex-col overflow-hidden" ]
                [ div [ class "flex items-center justify-between px-4 py-3 border-b border-base-300 bg-base-200/50" ]
                    [ h3 [ class "font-semibold text-base" ] [ text "Model Before Message" ]
                    , viewCollapseExpandButtons CollapseAllBefore ExpandAllBefore
                    ]
                , div [ class "flex-1 overflow-auto p-4" ]
                    [ if isFirstMessage then
                        viewInitialStateMessage

                      else if showDiffHighlighting then
                        TreeView.viewDiffBefore beforeDiffConfig data.modelBefore viewState.beforeExpandedPaths

                      else
                        TreeView.viewUnified unifiedConfigBefore data.modelBefore viewState.beforeExpandedPaths
                    ]
                ]

            -- After (right) panel
            , div [ class "bg-base-100 rounded-lg border border-base-300 flex flex-col overflow-hidden" ]
                [ div [ class "flex items-center justify-between px-4 py-3 border-b border-base-300 bg-base-200/50" ]
                    [ h3 [ class "font-semibold text-base" ] [ text "Model After Message" ]
                    , div [ class "flex items-center gap-2" ]
                        [ if showDiffHighlighting && changeCount > 0 then
                            span [ class "badge badge-warning badge-sm" ]
                                [ text (String.fromInt changeCount ++ " changes") ]

                          else if showDiffHighlighting then
                            span [ class "badge badge-success badge-sm" ]
                                [ text "No changes" ]

                          else
                            text ""
                        , viewCollapseExpandButtons CollapseAllAfter ExpandAllAfter
                        ]
                    ]
                , div [ class "flex-1 overflow-auto p-4" ]
                    [ if showDiffHighlighting then
                        TreeView.viewDiff diffConfig data.modelAfter viewState.afterExpandedPaths

                      else
                        TreeView.viewUnified unifiedConfigAfter data.modelAfter viewState.afterExpandedPaths
                    ]
                ]
            ]
        , if showDiffHighlighting then
            viewDiffLegend

          else
            text ""
        ]


viewInitialStateMessage : Html Msg
viewInitialStateMessage =
    div [ class "flex items-center justify-center h-full" ]
        [ div [ class "text-center text-base-content/60" ]
            [ div [ class "text-4xl mb-3" ] [ text "🎬" ]
            , p [ class "text-sm font-medium" ] [ text "Initial State" ]
            , p [ class "text-xs mt-1" ] [ text "This is the first message - no previous state available" ]
            ]
        ]


viewCollapseExpandButtons : Msg -> Msg -> Html Msg
viewCollapseExpandButtons collapseMsg expandMsg =
    div [ class "flex items-center gap-1" ]
        [ div [ class "tooltip tooltip-bottom", attribute "data-tip" "Collapse all" ]
            [ button
                [ class "btn btn-ghost btn-sm px-2 text-lg"
                , onClick collapseMsg
                ]
                [ text "\u{229F}" ]
            ]
        , div [ class "tooltip tooltip-bottom", attribute "data-tip" "Expand all" ]
            [ button
                [ class "btn btn-ghost btn-sm px-2 text-lg"
                , onClick expandMsg
                ]
                [ text "\u{229E}" ]
            ]
        ]


{-| Render a small quick-add filter icon.
-}
viewQuickAddIcon : Msg -> String -> Html Msg
viewQuickAddIcon msg tooltip =
    div [ class "tooltip tooltip-bottom", attribute "data-tip" tooltip ]
        [ button
            [ class "btn btn-ghost btn-xs px-0.5 text-base-content/30 hover:text-primary"
            , onClick msg
            ]
            [ i [ class "fa-solid fa-filter text-xs" ] [] ]
        ]


{-| Quick-add callback for model tree view fields.
-}
quickAddModelFilter : List String -> String -> Msg
quickAddModelFilter path value =
    QuickAddFilter (ModelValueFilter { key = String.join "." path, value = value })


{-| Quick-add callback for message payload tree view fields.
-}
quickAddMessageFieldFilter : List String -> String -> Msg
quickAddMessageFieldFilter path value =
    QuickAddFilter (MessageFieldFilter { key = String.join "." path, value = value })


{-| Quick-add callback for effect field tree view.
-}
quickAddEffectFieldFilter : List String -> String -> Msg
quickAddEffectFieldFilter path value =
    QuickAddFilter (EffectFieldFilter { key = String.join "." path, value = value })


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
-}
viewMessageDetailsPanel : Model -> Html Msg
viewMessageDetailsPanel model =
    case model.selectedIndex of
        Nothing ->
            text ""

        Just index ->
            case Array.get index model.logEntries of
                Nothing ->
                    text ""

                Just entry ->
                    case entry of
                        UpdateEntry data ->
                            viewMessageDetails model data

                        _ ->
                            text ""


viewMessageDetails : Model -> UpdateEntryData -> Html Msg
viewMessageDetails model data =
    let
        viewState =
            getMessageViewState model

        currentMatchPath =
            getCurrentMatchPath model

        config : TreeView.UnifiedConfig Msg
        config =
            { onToggleExpand = SetPayloadExpanded
            , searchMatches = model.searchResult.payloadPathsWithMatches
            , currentMatchPath = currentMatchPath
            , onQuickAddFilter = Just quickAddMessageFieldFilter
            }

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
                        [ text data.message.name ]
                    , viewQuickAddIcon (QuickAddFilter (MessageNameFilter { query = data.message.name })) "Filter by this message name"
                    ]
                , viewCollapseExpandButtons CollapseAllPayload ExpandAllPayload
                ]
            , div [ class "p-3" ]
                [ TreeView.viewUnified config data.message.payload viewState.payloadExpandedPaths ]
            ]
        ]


{-| Render the effects panel section.
-}
viewEffectsPanel : Model -> Html Msg
viewEffectsPanel model =
    let
        selectedEffects =
            case model.selectedIndex of
                Just index ->
                    Array.get index model.logEntries
                        |> Maybe.andThen getEffects
                        |> Maybe.withDefault []

                Nothing ->
                    []

        effectCount =
            List.length selectedEffects

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


viewEffectsList : Model -> Dict Int (Set String) -> List Effect -> Html Msg
viewEffectsList model effectExpandedPaths effects =
    if List.isEmpty effects then
        p [ class "text-sm text-base-content/60 py-2" ]
            [ text "No effects for this message" ]

    else
        ul [ class "space-y-2 pt-2" ]
            (List.indexedMap (viewEffectItem model effectExpandedPaths) effects)


viewEffectItem : Model -> Dict Int (Set String) -> Int -> Effect -> Html Msg
viewEffectItem model effectExpandedPaths index effect =
    let
        expandedPaths =
            Dict.get index effectExpandedPaths
                |> Maybe.withDefault (Set.singleton "")

        effectDataMatches =
            Dict.get index model.searchResult.effectDataPathsWithMatches
                |> Maybe.withDefault Set.empty

        currentMatchPath =
            getCurrentMatchPath model

        config : TreeView.UnifiedConfig Msg
        config =
            { onToggleExpand = SetEffectExpanded index
            , searchMatches = effectDataMatches
            , currentMatchPath = currentMatchPath
            , onQuickAddFilter = Just quickAddEffectFieldFilter
            }

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
                , viewQuickAddIcon (QuickAddFilter (EffectNameFilter { query = effect.name })) "Filter by this effect name"
                ]
            , viewCollapseExpandButtons (CollapseAllEffect index) (ExpandAllEffect index)
            ]
        , div [ class "p-3" ]
            [ TreeView.viewUnified config effect.data expandedPaths ]
        ]


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


keyboardShortcutDecoder : D.Decoder Msg
keyboardShortcutDecoder =
    D.map4
        (\key metaKey ctrlKey tagName ->
            let
                isInputElement =
                    tagName == "INPUT" || tagName == "TEXTAREA" || tagName == "SELECT"
            in
            if key == "f" && (metaKey || ctrlKey) then
                FocusSearch

            else if key == "ArrowDown" && not isInputElement then
                SelectNextMessage

            else if key == "ArrowUp" && not isInputElement then
                SelectPreviousMessage

            else
                NoOp
        )
        (D.field "key" D.string)
        (D.field "metaKey" D.bool)
        (D.field "ctrlKey" D.bool)
        (D.at [ "target", "tagName" ] D.string)
