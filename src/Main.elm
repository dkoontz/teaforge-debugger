module Main exposing (main)

{-| Main entry point for the TeaForge Debugger application.

This module implements The Elm Architecture (TEA) with Model, Msg, init, update,
view, and subscriptions. It provides the application shell and coordinates
communication with JavaScript via ports.

-}

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import Ports
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

-}
type alias Model =
    { logEntries : List LogEntry
    , selectedIndex : Maybe Int
    , viewMode : ViewMode
    , searchQuery : String
    , searchMatches : List TreePath
    , currentMatchIndex : Int
    , filterActive : Bool
    , sidebarWidth : Int
    , loadingState : LoadingState
    , errorMessage : Maybe String
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
      , currentMatchIndex = 0
      , filterActive = False
      , sidebarWidth = 320
      , loadingState = Idle
      , errorMessage = Nothing
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
                    Just _ ->
                        -- TODO: Parse log file contents in phase-5
                        ( { model
                            | loadingState = Loaded
                            , logEntries = []
                            , selectedIndex = Nothing
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
            ( { model | selectedIndex = Just index }
            , Cmd.none
            )

        -- View Mode
        SetViewMode mode ->
            ( { model | viewMode = mode }
            , Cmd.none
            )

        -- Search
        SetSearchQuery query ->
            ( { model
                | searchQuery = query
                , searchMatches = []
                , currentMatchIndex = 0
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



-- VIEW


{-| Main view function rendering the application layout.

Uses DaisyUI components for styling:

  - Drawer for sidebar layout
  - Menu for message list
  - Tabs for view mode switching

-}
view : Model -> Html Msg
view model =
    div [ class "drawer drawer-open h-full" ]
        [ -- Drawer toggle (hidden, drawer is always open on desktop)
          input
            [ id "sidebar-drawer"
            , type_ "checkbox"
            , class "drawer-toggle"
            , checked True
            ]
            []

        -- Main content area
        , div [ class "drawer-content flex flex-col" ]
            [ viewHeader model
            , viewMainContent model
            ]

        -- Sidebar
        , div [ class "drawer-side h-full" ]
            [ label
                [ for "sidebar-drawer"
                , attribute "aria-label" "close sidebar"
                , class "drawer-overlay"
                ]
                []
            , viewSidebar model
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
-}
viewSidebar : Model -> Html Msg
viewSidebar model =
    aside
        [ class "bg-base-200 w-80 min-h-full flex flex-col"
        , style "width" (String.fromInt model.sidebarWidth ++ "px")
        ]
        [ div [ class "p-4 border-b border-base-300" ]
            [ h2 [ class "font-semibold text-lg" ] [ text "Messages" ]
            , span [ class "text-sm text-base-content/60" ]
                [ text (String.fromInt (List.length model.logEntries) ++ " messages") ]
            ]
        , viewMessageList model
        ]


{-| Render the list of messages in the sidebar.
-}
viewMessageList : Model -> Html Msg
viewMessageList model =
    if List.isEmpty model.logEntries then
        div [ class "flex-1 flex items-center justify-center p-4" ]
            [ div [ class "text-center text-base-content/60" ]
                [ p [ class "text-sm" ] [ text "No messages loaded" ]
                , p [ class "text-xs mt-2" ] [ text "Open a TeaForge log file to begin" ]
                ]
            ]

    else
        ul [ class "menu p-2 flex-1 overflow-y-auto" ]
            (List.indexedMap (viewMessageItem model.selectedIndex) model.logEntries)


{-| Render a single message item in the list.
-}
viewMessageItem : Maybe Int -> Int -> LogEntry -> Html Msg
viewMessageItem selectedIndex index entry =
    let
        isSelected =
            selectedIndex == Just index

        itemClass =
            if isSelected then
                "menu-item-active"

            else
                ""
    in
    li []
        [ a
            [ class ("flex flex-col items-start " ++ itemClass)
            , onClick (SelectMessage index)
            ]
            [ span [ class "font-medium" ] [ text entry.message.name ]
            , span [ class "text-xs text-base-content/60" ]
                [ text (formatTimestamp entry.timestamp) ]
            ]
        ]


{-| Format a Unix timestamp for display.

TODO: Implement proper timestamp formatting in a utility module.

-}
formatTimestamp : Int -> String
formatTimestamp timestamp =
    -- Simple placeholder formatting
    String.fromInt timestamp


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
-}
viewSearchBox : Model -> Html Msg
viewSearchBox model =
    div [ class "flex items-center gap-2" ]
        [ div [ class "form-control" ]
            [ input
                [ type_ "text"
                , placeholder "Search..."
                , class "input input-bordered input-sm w-48"
                , value model.searchQuery
                , onInput SetSearchQuery
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


{-| Render search navigation buttons and match counter.
-}
viewSearchNavigation : Model -> Html Msg
viewSearchNavigation model =
    let
        matchCount =
            List.length model.searchMatches

        hasMatches =
            matchCount > 0 && not (String.isEmpty model.searchQuery)
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
                    (String.fromInt (model.currentMatchIndex + 1)
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
-}
viewStateContent : Model -> Html Msg
viewStateContent model =
    div [ class "flex-1 overflow-auto p-4" ]
        [ case model.selectedIndex of
            Nothing ->
                viewNoSelection

            Just _ ->
                viewSelectedState model
        ]


{-| Render placeholder when no message is selected.
-}
viewNoSelection : Html Msg
viewNoSelection =
    div [ class "h-full flex items-center justify-center" ]
        [ div [ class "text-center text-base-content/60" ]
            [ p [ class "text-lg" ] [ text "No message selected" ]
            , p [ class "text-sm mt-2" ] [ text "Select a message from the sidebar to view its state" ]
            ]
        ]


{-| Render the state view for the selected message.

TODO: Implement actual tree view in phase-7.

-}
viewSelectedState : Model -> Html Msg
viewSelectedState model =
    case model.viewMode of
        PostState ->
            div [ class "bg-base-100 rounded-lg border border-base-300 p-4" ]
                [ p [ class "text-base-content/60" ]
                    [ text "State tree view will be implemented in phase-7" ]
                ]

        SplitView ->
            div [ class "grid grid-cols-2 gap-4 h-full" ]
                [ div [ class "bg-base-100 rounded-lg border border-base-300 p-4" ]
                    [ h3 [ class "font-semibold mb-2" ] [ text "Before" ]
                    , p [ class "text-base-content/60 text-sm" ]
                        [ text "Split view will be implemented in phase-9" ]
                    ]
                , div [ class "bg-base-100 rounded-lg border border-base-300 p-4" ]
                    [ h3 [ class "font-semibold mb-2" ] [ text "After" ]
                    , p [ class "text-base-content/60 text-sm" ]
                        [ text "Split view will be implemented in phase-9" ]
                    ]
                ]

        DiffView _ ->
            div [ class "bg-base-100 rounded-lg border border-base-300 p-4" ]
                [ p [ class "text-base-content/60" ]
                    [ text "Diff view will be implemented in phase-9" ]
                ]


{-| Render the effects footer section.

TODO: Implement actual effects display in phase-8.

-}
viewEffectsFooter : Model -> Html Msg
viewEffectsFooter model =
    div [ class "border-t border-base-300 bg-base-200" ]
        [ div [ class "collapse collapse-arrow" ]
            [ input [ type_ "checkbox", class "peer" ] []
            , div [ class "collapse-title font-medium" ]
                [ text "Effects"
                , case model.selectedIndex of
                    Just index ->
                        let
                            effectCount =
                                model.logEntries
                                    |> List.drop index
                                    |> List.head
                                    |> Maybe.map (.effects >> List.length)
                                    |> Maybe.withDefault 0
                        in
                        span [ class "badge badge-sm ml-2" ]
                            [ text (String.fromInt effectCount) ]

                    Nothing ->
                        text ""
                ]
            , div [ class "collapse-content" ]
                [ p [ class "text-sm text-base-content/60" ]
                    [ text "Effects display will be implemented in phase-8" ]
                ]
            ]
        ]



-- SUBSCRIPTIONS


{-| Application subscriptions.

Subscribes to the incoming port for JavaScript messages.

-}
subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.incoming GotPortMessage
