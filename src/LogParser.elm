module LogParser exposing
    ( parseLogFile
    , parseLogEntry
    , ParseResult
    , ParseError(..)
    , logEntryDecoder
    , messageDataDecoder
    , effectDecoder
    )

{-| TeaForge log file parser.

This module provides JSON decoders for parsing TeaForge log files.
Log files contain a list of entries, each representing one message
processed by the TEA update function.

**Note:** The TeaForge log format is assumed based on typical Elm Architecture patterns.
This parser is designed to be easily modified once the real format is confirmed.

## Parsing

    parseLogFile : String -> ParseResult (List LogEntry)
    parseLogEntry : D.Value -> Result ParseError LogEntry

## Decoders

    logEntryDecoder : D.Decoder LogEntry
    messageDataDecoder : D.Decoder MessageData
    effectDecoder : D.Decoder Effect

-}

import Json.Decode as D
import Json.Decode.Pipeline as P
import Json.Encode as E
import Types exposing (Effect, LogEntry, MessageData)


{-| Result of parsing a log file.

Contains either:

  - `Ok` with successfully parsed entries and a count of skipped malformed entries
  - `Err` with a parse error describing what went wrong

-}
type alias ParseResult a =
    Result ParseError ( a, Int )


{-| Errors that can occur during parsing.
-}
type ParseError
    = InvalidJson String
    | UnexpectedFormat String
    | EmptyFile


{-| Parse a complete log file from a JSON string.

Returns the list of successfully parsed entries along with a count
of malformed entries that were skipped.

    parseLogFile "{\"entries\": [...]}"
    -- Ok ( [ entry1, entry2 ], 0 )

    parseLogFile "not json"
    -- Err (InvalidJson "...")

-}
parseLogFile : String -> ParseResult (List LogEntry)
parseLogFile content =
    if String.trim content == "" then
        Err EmptyFile

    else
        case D.decodeString logFileDecoder content of
            Ok entries ->
                Ok entries

            Err jsonError ->
                -- Try alternative formats
                case D.decodeString alternativeLogFileDecoder content of
                    Ok entries ->
                        Ok entries

                    Err _ ->
                        Err (InvalidJson (D.errorToString jsonError))


{-| Parse a single log entry from a JSON Value.

Useful for parsing entries one at a time or testing.

-}
parseLogEntry : D.Value -> Result ParseError LogEntry
parseLogEntry value =
    case D.decodeValue logEntryDecoder value of
        Ok entry ->
            Ok entry

        Err jsonError ->
            Err (InvalidJson (D.errorToString jsonError))



-- DECODERS


{-| Decoder for the main log file format.

Expected format (primary):

    {
        "entries": [
            { "timestamp": 1234567890, "message": {...}, ... },
            ...
        ]
    }

-}
logFileDecoder : D.Decoder ( List LogEntry, Int )
logFileDecoder =
    D.field "entries" (lenientListDecoder logEntryDecoder)


{-| Alternative decoder for array-based log format.

Expected format (alternative):

    [
        { "timestamp": 1234567890, "message": {...}, ... },
        ...
    ]

-}
alternativeLogFileDecoder : D.Decoder ( List LogEntry, Int )
alternativeLogFileDecoder =
    lenientListDecoder logEntryDecoder


{-| Decoder for a single log entry.

Expected format:

    {
        "timestamp": 1234567890,
        "message": {
            "name": "ButtonClicked",
            "payload": { ... }
        },
        "modelBefore": { ... },
        "modelAfter": { ... },
        "effects": [
            { "name": "Http", "data": { ... } }
        ]
    }

Handles variations in field naming:

  - `modelBefore` or `stateBefore` or `before`
  - `modelAfter` or `stateAfter` or `after`
  - `effects` or `commands` or `cmds`

-}
logEntryDecoder : D.Decoder LogEntry
logEntryDecoder =
    D.succeed LogEntry
        |> P.custom timestampDecoder
        |> P.required "message" messageDataDecoder
        |> P.custom modelBeforeDecoder
        |> P.custom modelAfterDecoder
        |> P.custom effectsDecoder


{-| Decoder for message data.

Expected format:

    {
        "name": "ButtonClicked",
        "payload": { "buttonId": "submit" }
    }

Or simplified:

    {
        "name": "ButtonClicked",
        "data": { ... }
    }

Or just the message type:

    {
        "type": "ButtonClicked"
    }

-}
messageDataDecoder : D.Decoder MessageData
messageDataDecoder =
    D.oneOf
        [ -- Standard format: name + payload
          D.map2 MessageData
            (D.field "name" D.string)
            (D.oneOf
                [ D.field "payload" D.value
                , D.field "data" D.value
                , D.succeed nullValue
                ]
            )
        , -- Alternative: type field instead of name
          D.map2 MessageData
            (D.field "type" D.string)
            (D.oneOf
                [ D.field "payload" D.value
                , D.field "data" D.value
                , D.succeed nullValue
                ]
            )
        ]


{-| Decoder for an effect (command).

Expected format:

    {
        "name": "Http",
        "data": {
            "url": "https://api.example.com",
            "method": "GET"
        }
    }

-}
effectDecoder : D.Decoder Effect
effectDecoder =
    D.oneOf
        [ -- Standard format: name + data
          D.map2 Effect
            (D.field "name" D.string)
            (D.oneOf
                [ D.field "data" D.value
                , D.field "payload" D.value
                , D.succeed nullValue
                ]
            )
        , -- Alternative: type + payload/data
          D.map2 Effect
            (D.field "type" D.string)
            (D.oneOf
                [ D.field "data" D.value
                , D.field "payload" D.value
                , D.succeed nullValue
                ]
            )
        , -- Just a string command name
          D.string
            |> D.map (\name -> { name = name, data = nullValue })
        ]



-- HELPER DECODERS


{-| Decode a timestamp, handling both integer and string formats.

Tries multiple field names for compatibility.

-}
timestampDecoder : D.Decoder Int
timestampDecoder =
    D.oneOf
        [ D.field "timestamp" D.int
        , D.field "timestamp" D.string
            |> D.andThen
                (\str ->
                    case String.toInt str of
                        Just n ->
                            D.succeed n

                        Nothing ->
                            D.fail ("Invalid timestamp: " ++ str)
                )
        , D.field "time" D.int
        , D.field "ts" D.int
        , -- If timestamp is missing, use 0
          D.succeed 0
        ]


{-| Decoder for model state before message processing.

Tries multiple field names for compatibility.

-}
modelBeforeDecoder : D.Decoder D.Value
modelBeforeDecoder =
    D.oneOf
        [ D.field "modelBefore" D.value
        , D.field "stateBefore" D.value
        , D.field "before" D.value
        , D.field "previousState" D.value
        , D.field "prevModel" D.value
        , D.succeed nullValue
        ]


{-| Decoder for model state after message processing.

Tries multiple field names for compatibility.

-}
modelAfterDecoder : D.Decoder D.Value
modelAfterDecoder =
    D.oneOf
        [ D.field "modelAfter" D.value
        , D.field "stateAfter" D.value
        , D.field "after" D.value
        , D.field "nextState" D.value
        , D.field "newModel" D.value
        , D.field "model" D.value
        , D.field "state" D.value
        , D.succeed nullValue
        ]


{-| Decoder for effects list.

Tries multiple field names and handles missing effects.

-}
effectsDecoder : D.Decoder (List Effect)
effectsDecoder =
    D.oneOf
        [ D.field "effects" (D.list effectDecoder)
        , D.field "commands" (D.list effectDecoder)
        , D.field "cmds" (D.list effectDecoder)
        , D.succeed []
        ]


{-| A lenient list decoder that skips malformed entries.

Returns a tuple of (successfully parsed items, count of skipped items).

-}
lenientListDecoder : D.Decoder a -> D.Decoder ( List a, Int )
lenientListDecoder itemDecoder =
    D.list D.value
        |> D.map (partitionDecodeResults itemDecoder)


{-| Attempt to decode each value in a list, collecting successes and counting failures.
-}
partitionDecodeResults : D.Decoder a -> List D.Value -> ( List a, Int )
partitionDecodeResults decoder values =
    let
        results =
            List.map (D.decodeValue decoder) values

        successes =
            List.filterMap Result.toMaybe results

        failureCount =
            List.length values - List.length successes
    in
    ( successes, failureCount )



-- UTILITIES


{-| A null JSON value.

Used as a fallback for optional fields.

-}
nullValue : D.Value
nullValue =
    E.null
