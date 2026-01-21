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

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types exposing (LogEntry)


{-| Configuration for the MessageList component.

  - `selectedIndex`: Currently selected message index, if any
  - `onSelect`: Callback when a message is selected
  - `entries`: List of log entries to display

-}
type alias Config msg =
    { selectedIndex : Maybe Int
    , onSelect : Int -> msg
    , entries : List LogEntry
    }


{-| Render the complete message list.

Displays either the list of messages or an empty state placeholder.
Uses DaisyUI menu component styling.

-}
view : Config msg -> Html msg
view config =
    if List.isEmpty config.entries then
        viewEmpty

    else
        ul [ class "menu p-2 flex-1 overflow-y-auto" ]
            (List.indexedMap (viewItem config.selectedIndex config.onSelect) config.entries)


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
for the selection state.

-}
viewItem : Maybe Int -> (Int -> msg) -> Int -> LogEntry -> Html msg
viewItem selectedIndex onSelect index entry =
    let
        isSelected =
            selectedIndex == Just index

        activeClass =
            if isSelected then
                "active"

            else
                ""
    in
    li []
        [ a
            [ class ("flex flex-col items-start gap-0.5 " ++ activeClass)
            , onClick (onSelect index)
            , attribute "role" "button"
            , tabindex 0
            ]
            [ span [ class "font-medium truncate w-full" ]
                [ text entry.message.name ]
            , span [ class "text-xs text-base-content/60" ]
                [ text (formatTimestamp entry.timestamp) ]
            ]
        ]


{-| Format a Unix timestamp for display.

Converts the timestamp to a human-readable format showing time.
For now, displays the raw timestamp value. This will be enhanced
in a later phase to show formatted date/time.

-}
formatTimestamp : Int -> String
formatTimestamp timestamp =
    -- Format as time string if timestamp looks like milliseconds
    -- Otherwise just show the raw value
    if timestamp > 1000000000000 then
        -- Likely milliseconds since epoch, show simplified time
        let
            -- Extract hours, minutes, seconds from milliseconds
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

    else if timestamp > 1000000000 then
        -- Likely seconds since epoch
        let
            totalMinutes =
                timestamp // 60

            seconds =
                modBy 60 timestamp

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

    else
        -- Small number, might be sequence number or relative time
        "#" ++ String.fromInt timestamp
