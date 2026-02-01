module MessageList exposing
    ( Config
    , view
    , viewEmpty
    , viewItem
    )

{-| Message list component for the TeaForge Debugger sidebar.

This module provides a DaisyUI-styled menu component for displaying
log entries in chronological order. It handles selection state and
provides callback configuration for user interactions.

@docs Config, view, viewEmpty, viewItem

-}

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types exposing (DisplayOrder(..), LogEntry(..), getMessageName, getTimestamp)


{-| Configuration for the MessageList component.

  - `selectedIndex`: Currently selected message index, if any
  - `onSelect`: Callback when a message is selected
  - `entries`: Array of log entries to display
  - `displayOrder`: How to order messages (newest or oldest first)

-}
type alias Config msg =
    { selectedIndex : Maybe Int
    , onSelect : Int -> msg
    , entries : Array LogEntry
    , displayOrder : DisplayOrder
    }


{-| Render the complete message list.

Displays messages based on the configured display order:

  - ReverseChronological: newest at top (default)
  - Chronological: oldest at top

Uses DaisyUI menu component styling.

-}
view : Config msg -> Html msg
view config =
    if Array.isEmpty config.entries then
        viewEmpty

    else
        let
            totalCount =
                Array.length config.entries

            -- Build list of (displayIndex, originalIndex, entry) tuples
            indexedEntries =
                Array.toIndexedList config.entries

            -- For ReverseChronological: reverse entries (newest first)
            -- For Chronological: keep original order (oldest first)
            ( displayEntries, indexMapper ) =
                case config.displayOrder of
                    ReverseChronological ->
                        -- Reverse entries, map display index back to original
                        ( List.reverse indexedEntries
                        , \displayIndex -> totalCount - 1 - displayIndex
                        )

                    Chronological ->
                        -- Keep original order, display index equals original index
                        ( indexedEntries
                        , \displayIndex -> displayIndex
                        )

            viewItemWithDisplayIndex displayIndex ( originalIndex, entry ) =
                viewItem config.selectedIndex config.onSelect originalIndex entry
        in
        ul [ class "flex flex-col gap-1 p-2 flex-1 overflow-y-auto overflow-x-hidden min-w-0" ]
            (List.indexedMap viewItemWithDisplayIndex displayEntries)


{-| Render the empty state when no messages are loaded.

Shows a centered placeholder message prompting the user to load a file.

-}
viewEmpty : Html msg
viewEmpty =
    div [ class "flex-1 flex items-center justify-center p-4" ]
        [ div [ class "text-center text-base-content/60" ]
            [ p [ class "text-sm" ] [ text "No messages loaded" ]
            , p [ class "text-xs mt-2" ] [ text "Open a TeaForge log file to begin" ]
            ]
        ]


{-| Render a single message item in the list.

Displays the message name and timestamp with appropriate styling
for the selection state. Selected items have a highlighted background
and a left border indicator.

-}
viewItem : Maybe Int -> (Int -> msg) -> Int -> LogEntry -> Html msg
viewItem selectedIndex onSelect index entry =
    let
        isSelected =
            selectedIndex == Just index

        isError =
            case entry of
                ErrorEntry _ ->
                    True

                _ ->
                    False

        itemClasses =
            if isSelected then
                "block px-3 py-1.5 rounded-lg bg-primary/10 border-l-2 border-primary cursor-pointer"

            else if isError then
                "block px-3 py-1.5 rounded-lg hover:bg-error/10 border-l-2 border-error/50 cursor-pointer"

            else
                "block px-3 py-1.5 rounded-lg hover:bg-base-300 border-l-2 border-transparent cursor-pointer"

        textClasses =
            if isSelected then
                "block font-medium text-primary text-sm overflow-hidden whitespace-nowrap"

            else if isError then
                "block font-medium text-error text-sm overflow-hidden whitespace-nowrap"

            else
                "block font-medium text-sm overflow-hidden whitespace-nowrap"

        messageName =
            getMessageName entry

        timestampText =
            case entry of
                ErrorEntry data ->
                    "Line " ++ String.fromInt data.lineNumber

                _ ->
                    case getTimestamp entry of
                        Just ts ->
                            "#" ++ String.fromInt (index + 1) ++ " · " ++ formatTimestamp ts

                        Nothing ->
                            "#" ++ String.fromInt (index + 1)

        iconHtml =
            case entry of
                ErrorEntry _ ->
                    span [ class "mr-1" ]
                        [ i [ class "fa-solid fa-triangle-exclamation text-error" ] [] ]

                InitEntry _ ->
                    span [ class "mr-1" ]
                        [ i [ class "fa-solid fa-play text-success" ] [] ]

                SubscriptionChangeEntry _ ->
                    span [ class "mr-1" ]
                        [ i [ class "fa-solid fa-arrows-rotate text-info" ] [] ]

                UpdateEntry _ ->
                    text ""
    in
    li [ class "min-w-0" ]
        [ a
            [ id ("message-item-" ++ String.fromInt index)
            , class itemClasses
            , onClick (onSelect index)
            , attribute "role" "button"
            , tabindex 0
            ]
            [ span
                [ class textClasses
                , style "text-overflow" "ellipsis"
                ]
                [ iconHtml
                , text messageName
                ]
            , span
                [ class "block text-xs text-base-content/60 overflow-hidden whitespace-nowrap"
                , style "text-overflow" "ellipsis"
                ]
                [ text timestampText ]
            ]
        ]


{-| Format a Unix timestamp (milliseconds since epoch) for display.

Converts the timestamp to a human-readable time format with millisecond precision.

-}
formatTimestamp : Int -> String
formatTimestamp timestamp =
    let
        milliseconds =
            modBy 1000 timestamp

        totalSeconds =
            timestamp // 1000

        seconds =
            modBy 60 totalSeconds

        totalMinutes =
            totalSeconds // 60

        minutes =
            modBy 60 totalMinutes

        hours =
            modBy 24 (totalMinutes // 60)
    in
    String.padLeft 2 '0' (String.fromInt hours)
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt minutes)
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt seconds)
        ++ "."
        ++ String.padLeft 3 '0' (String.fromInt milliseconds)
