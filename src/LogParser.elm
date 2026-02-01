module LogParser exposing
    ( parseLogFile
    , parseLogEntry
    , ParseResult
    , ParseError(..)
    , MalformedEntryInfo
    , logEntryDecoder
    , messageDataDecoder
    , effectDecoder
    )

{-| TeaForge log file parser.

This module provides JSON decoders for parsing TeaForge JSONL log files.
The log format uses one JSON object per line with entry types:
- `header` - Type registry and metadata (first line, skipped)
- `init` - Initial state and effects from init()
- `update` - Message processing result
- `subscriptionChange` - Subscription lifecycle events (skipped)

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
    = EmptyFile
    | MalformedEntry
        { lineNumber : Int
        , preview : String
        , reason : String
        }
    | NoValidEntries { malformedCount : Int, firstError : Maybe MalformedEntryInfo }
    | InvalidJson String
    | UnexpectedFormat String


{-| Information about a malformed entry for error reporting.
-}
type alias MalformedEntryInfo =
    { lineNumber : Int
    , preview : String
    , reason : String
    }


{-| Internal representation of a raw JSONL entry before processing.
-}
type RawEntry
    = InitEntry
        { sequence : Int
        , timestamp : Int
        , model : D.Value
        , effects : List Effect
        }
    | UpdateEntry
        { sequence : Int
        , timestamp : Int
        , message : MessageData
        , model : D.Value
        , effects : List Effect
        }
    | SkippedEntry


{-| Parse a complete log file from a JSONL string.

Returns the list of successfully parsed entries along with a count
of malformed entries that were skipped.

The parser handles:
- JSONL format (one JSON object per line)
- Legacy JSON array format
- Legacy { entries: [...] } format

For JSONL, the parser:
1. Skips `header` and `subscriptionChange` entries
2. Converts `init` and `update` entries to LogEntry
3. Reconstructs modelBefore from previous entry's model

-}
parseLogFile : String -> ParseResult (List LogEntry)
parseLogFile content =
    if String.trim content == "" then
        Err EmptyFile

    else if isJsonlFormat content then
        -- JSONL format with type field
        parseJsonlFormat content

    else
        -- Try legacy formats
        case D.decodeString legacyLogFileDecoder content of
            Ok entries ->
                Ok entries

            Err jsonError ->
                case D.decodeString legacyArrayDecoder content of
                    Ok entries ->
                        Ok entries

                    Err _ ->
                        -- Last resort: try JSONL anyway
                        parseJsonlFormat content


{-| Check if content appears to be JSONL format.
-}
isJsonlFormat : String -> Bool
isJsonlFormat content =
    let
        trimmed =
            String.trim content
    in
    String.startsWith "{\"type\"" trimmed


{-| Parse JSONL format (one JSON object per line).
-}
parseJsonlFormat : String -> ParseResult (List LogEntry)
parseJsonlFormat content =
    let
        lines =
            content
                |> String.lines
                |> List.indexedMap (\i line -> ( i + 1, line ))
                |> List.filter (\( _, line ) -> not (String.isEmpty (String.trim line)))
    in
    if List.isEmpty lines then
        Err EmptyFile

    else
        let
            parseResult =
                parseRawEntriesWithLineNumbers lines

            logEntries =
                convertRawEntriesToLogEntries parseResult.entries
        in
        if List.isEmpty logEntries && not (List.isEmpty parseResult.errors) then
            -- No valid entries found, report first error
            Err
                (NoValidEntries
                    { malformedCount = List.length parseResult.errors
                    , firstError = List.head parseResult.errors
                    }
                )

        else if List.isEmpty logEntries then
            -- No entries at all (file only had headers/subscriptionChanges)
            Err
                (NoValidEntries
                    { malformedCount = 0
                    , firstError = Nothing
                    }
                )

        else
            Ok ( logEntries, List.length parseResult.errors )


{-| Result of parsing raw entries with error tracking.
-}
type alias ParseEntriesResult =
    { entries : List RawEntry
    , errors : List MalformedEntryInfo
    }


{-| Parse each line into a RawEntry, tracking line numbers and collecting errors.
-}
parseRawEntriesWithLineNumbers : List ( Int, String ) -> ParseEntriesResult
parseRawEntriesWithLineNumbers numberedLines =
    let
        processLine ( lineNum, line ) acc =
            case parseRawEntryWithError lineNum line of
                Ok entry ->
                    { acc | entries = entry :: acc.entries }

                Err errorInfo ->
                    { acc | errors = errorInfo :: acc.errors }
    in
    List.foldl processLine { entries = [], errors = [] } numberedLines
        |> (\result ->
                { entries = List.reverse result.entries
                , errors = List.reverse result.errors
                }
           )


{-| Parse a single line into a RawEntry, returning detailed error on failure.
-}
parseRawEntryWithError : Int -> String -> Result MalformedEntryInfo RawEntry
parseRawEntryWithError lineNum line =
    let
        trimmed =
            String.trim line
    in
    case D.decodeString rawEntryDecoder trimmed of
        Ok entry ->
            Ok entry

        Err decodeErr ->
            Err
                { lineNumber = lineNum
                , preview = truncatePreview trimmed
                , reason = decodeErrorToReason decodeErr
                }


{-| Convert a JSON decode error to a human-readable reason.
-}
decodeErrorToReason : D.Error -> String
decodeErrorToReason error =
    case error of
        D.Field field subError ->
            "Missing or invalid field '" ++ field ++ "'"

        D.Index idx subError ->
            "Error at index " ++ String.fromInt idx

        D.OneOf errors ->
            "No valid format matched"

        D.Failure message _ ->
            message


{-| Truncate a line to create a preview for error messages.
-}
truncatePreview : String -> String
truncatePreview line =
    let
        maxLen =
            30

        trimmed =
            String.trim line
    in
    if String.length trimmed <= maxLen then
        trimmed

    else
        String.left maxLen trimmed ++ "..."


{-| Convert raw entries to LogEntries, reconstructing modelBefore from previous entry.
-}
convertRawEntriesToLogEntries : List RawEntry -> List LogEntry
convertRawEntriesToLogEntries rawEntries =
    let
        -- Filter out SkippedEntry and keep only init/update
        validEntries =
            List.filter ((/=) SkippedEntry) rawEntries

        -- Convert with previous model tracking
        convertWithPrevious : D.Value -> List RawEntry -> List LogEntry -> List LogEntry
        convertWithPrevious prevModel remaining acc =
            case remaining of
                [] ->
                    List.reverse acc

                (InitEntry entry) :: rest ->
                    let
                        logEntry =
                            { timestamp = entry.timestamp
                            , message = { name = "Init", payload = E.null }
                            , modelBefore = E.null
                            , modelAfter = entry.model
                            , effects = entry.effects
                            }
                    in
                    convertWithPrevious entry.model rest (logEntry :: acc)

                (UpdateEntry entry) :: rest ->
                    let
                        logEntry =
                            { timestamp = entry.timestamp
                            , message = entry.message
                            , modelBefore = prevModel
                            , modelAfter = entry.model
                            , effects = entry.effects
                            }
                    in
                    convertWithPrevious entry.model rest (logEntry :: acc)

                SkippedEntry :: rest ->
                    convertWithPrevious prevModel rest acc
    in
    convertWithPrevious E.null validEntries []


{-| Decoder for a raw JSONL entry.

Decodes the "type" field to determine entry type per the tea-debug-log schema.

-}
rawEntryDecoder : D.Decoder RawEntry
rawEntryDecoder =
    D.field "type" D.string
        |> D.andThen
            (\entryType ->
                case entryType of
                    "header" ->
                        D.succeed SkippedEntry

                    "subscriptionChange" ->
                        D.succeed SkippedEntry

                    "init" ->
                        initEntryDecoder

                    "update" ->
                        updateEntryDecoder

                    _ ->
                        D.succeed SkippedEntry
            )


{-| Decoder for init entry.
-}
initEntryDecoder : D.Decoder RawEntry
initEntryDecoder =
    D.map4
        (\seq ts model effs ->
            InitEntry
                { sequence = seq
                , timestamp = ts
                , model = model
                , effects = effs
                }
        )
        (D.oneOf [ D.field "sequence" D.int, D.succeed 0 ])
        (D.oneOf [ D.field "timestamp" D.int, D.succeed 0 ])
        (D.field "model" D.value)
        (D.oneOf [ D.field "effects" (D.list effectDecoder), D.succeed [] ])


{-| Decoder for update entry.
-}
updateEntryDecoder : D.Decoder RawEntry
updateEntryDecoder =
    D.map5
        (\seq ts msg model effs ->
            UpdateEntry
                { sequence = seq
                , timestamp = ts
                , message = msg
                , model = model
                , effects = effs
                }
        )
        (D.oneOf [ D.field "sequence" D.int, D.succeed 0 ])
        (D.oneOf [ D.field "timestamp" D.int, D.succeed 0 ])
        (D.field "message" newMessageDataDecoder)
        (D.field "model" D.value)
        (D.oneOf [ D.field "effects" (D.list effectDecoder), D.succeed [] ])


{-| Decoder for message data in new format.

The new format uses `_type` for the message type name.
If there's an `_unwrapped` or `_inner` field, that contains the inner message.

-}
newMessageDataDecoder : D.Decoder MessageData
newMessageDataDecoder =
    D.oneOf
        [ -- New format with _type and possibly _unwrapped/_inner
          D.andThen
            (\typeStr ->
                D.oneOf
                    [ -- Has _unwrapped - use the unwrapped message type
                      D.field "_unwrapped" D.value
                        |> D.andThen (decodeInnerMessage typeStr)
                    , -- Has _inner - use the inner message type
                      D.field "_inner" D.value
                        |> D.andThen (decodeInnerMessage typeStr)
                    , -- No _unwrapped/_inner - use the whole message as payload
                      D.value
                        |> D.map (\v -> { name = typeStr, payload = v })
                    ]
            )
            (D.field "_type" D.string)
        , -- Legacy format fallback
          messageDataDecoder
        ]


{-| Decode inner message, extracting its type name if available.
-}
decodeInnerMessage : String -> D.Value -> D.Decoder MessageData
decodeInnerMessage fallbackType inner =
    case D.decodeValue (D.field "_type" D.string) inner of
        Ok innerType ->
            D.succeed { name = innerType, payload = inner }

        Err _ ->
            D.succeed { name = fallbackType, payload = inner }


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



-- LEGACY DECODERS (for backwards compatibility)


{-| Decoder for the legacy log file format.

Expected format:

    {
        "entries": [
            { "timestamp": 1234567890, "message": {...}, ... },
            ...
        ]
    }

-}
legacyLogFileDecoder : D.Decoder ( List LogEntry, Int )
legacyLogFileDecoder =
    D.field "entries" (lenientListDecoder logEntryDecoder)


{-| Alternative decoder for legacy array-based log format.

Expected format:

    [
        { "timestamp": 1234567890, "message": {...}, ... },
        ...
    ]

-}
legacyArrayDecoder : D.Decoder ( List LogEntry, Int )
legacyArrayDecoder =
    lenientListDecoder logEntryDecoder


{-| Decoder for a single log entry (legacy format).

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

-}
logEntryDecoder : D.Decoder LogEntry
logEntryDecoder =
    D.succeed LogEntry
        |> P.custom timestampDecoder
        |> P.required "message" messageDataDecoder
        |> P.custom modelBeforeDecoder
        |> P.custom modelAfterDecoder
        |> P.custom effectsDecoder


{-| Decoder for message data (legacy format).

Expected format:

    {
        "name": "ButtonClicked",
        "payload": { "buttonId": "submit" }
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
        , -- New format: _type field
          D.map2 MessageData
            (D.field "_type" D.string)
            D.value
        ]


{-| Decoder for an effect (command).

Handles both new and legacy formats.

-}
effectDecoder : D.Decoder Effect
effectDecoder =
    D.oneOf
        [ -- New format: _type field identifies the effect type
          D.field "_type" D.string
            |> D.andThen
                (\typeStr ->
                    D.value
                        |> D.map (\v -> { name = typeStr, data = v })
                )
        , -- Legacy format: name + data
          D.map2 Effect
            (D.field "name" D.string)
            (D.oneOf
                [ D.field "data" D.value
                , D.field "payload" D.value
                , D.succeed nullValue
                ]
            )
        , -- Legacy: type + payload/data
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
        , D.field "sequence" D.int
        , D.succeed 0
        ]


{-| Decoder for model state before message processing (legacy).
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


{-| Decoder for model state after message processing (legacy).
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


{-| Decoder for effects list (legacy).
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
-}
nullValue : D.Value
nullValue =
    E.null
