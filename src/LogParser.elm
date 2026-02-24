module LogParser exposing
    ( entryTypeDecoder
    , initDataDecoder
    , updateDataDecoder
    , subscriptionChangeDataDecoder
    , effectDecoder
    , headerDecoder
    , HeaderData
    )

{-| TeaForge log entry decoders.

This module provides JSON decoders for parsing individual TeaForge log entries
from a streaming input source. Each entry is decoded based on its "type" field.

## Entry Type Detection

    entryTypeDecoder : D.Decoder String

## Entry Decoders

    initDataDecoder : D.Decoder InitData
    updateDataDecoder : D.Decoder UpdateData
    subscriptionChangeDataDecoder : D.Decoder SubscriptionChangeData

## Component Decoders

    effectDecoder : D.Decoder Effect

-}

import CompressionDict exposing (Compression)
import Diff
import Json.Decode as D
import Json.Encode as E
import Types exposing (Effect, MessageData)


{-| Intermediate type for decoded init entry data.
-}
type alias InitData =
    { timestamp : Int
    , modelDiff : List Diff.DiffOperation
    , effects : List Effect
    }


{-| Intermediate type for decoded update entry data.
-}
type alias UpdateData =
    { timestamp : Int
    , message : MessageData
    , modelDiff : List Diff.DiffOperation
    , effects : List Effect
    }


{-| Intermediate type for decoded subscription change entry data.
-}
type alias SubscriptionChangeData =
    { timestamp : Int
    , started : List D.Value
    , stopped : List D.Value
    }


{-| Data from a header entry.
-}
type alias HeaderData =
    { version : Int
    , compression : Maybe String
    }


{-| Decoder for the entry type field.
-}
entryTypeDecoder : D.Decoder String
entryTypeDecoder =
    D.field "type" D.string


{-| Decoder for header entry data.
-}
headerDecoder : D.Decoder HeaderData
headerDecoder =
    D.map2
        (\version compression ->
            { version = version
            , compression = compression
            }
        )
        (D.oneOf [ D.field "version" D.int, D.succeed 1 ])
        (D.oneOf [ D.field "compression" (D.map Just D.string), D.succeed Nothing ])


{-| Decoder for init entry data.

The decoder first decompresses the raw JSON value (to handle compressed keys like @0 for "_type"),
then decodes from the decompressed value.

-}
initDataDecoder : Compression -> D.Decoder InitData
initDataDecoder compression =
    D.value
        |> D.andThen
            (\rawValue ->
                let
                    decompressed =
                        CompressionDict.decompressValue compression rawValue
                in
                case D.decodeValue initDataDecoderInternal decompressed of
                    Ok data ->
                        D.succeed data

                    Err e ->
                        D.fail (D.errorToString e)
            )


{-| Internal decoder for init data (operates on already-decompressed JSON).
-}
initDataDecoderInternal : D.Decoder InitData
initDataDecoderInternal =
    D.map3
        (\ts modelDiff effs ->
            { timestamp = ts
            , modelDiff = modelDiff
            , effects = effs
            }
        )
        (D.oneOf [ D.field "timestamp" D.int, D.succeed 0 ])
        (D.field "modelDiff" (D.list Diff.diffOperationDecoder))
        (D.oneOf [ D.field "effects" (D.list effectDecoder), D.succeed [] ])


{-| Decoder for update entry data.

The decoder first decompresses the raw JSON value (to handle compressed keys like @0 for "_type"),
then decodes from the decompressed value.

-}
updateDataDecoder : Compression -> D.Decoder UpdateData
updateDataDecoder compression =
    D.value
        |> D.andThen
            (\rawValue ->
                let
                    decompressed =
                        CompressionDict.decompressValue compression rawValue
                in
                case D.decodeValue updateDataDecoderInternal decompressed of
                    Ok data ->
                        D.succeed data

                    Err e ->
                        D.fail (D.errorToString e)
            )


{-| Internal decoder for update data (operates on already-decompressed JSON).
-}
updateDataDecoderInternal : D.Decoder UpdateData
updateDataDecoderInternal =
    D.map4
        (\ts msg modelDiff effs ->
            { timestamp = ts
            , message = msg
            , modelDiff = modelDiff
            , effects = effs
            }
        )
        (D.oneOf [ D.field "timestamp" D.int, D.succeed 0 ])
        (D.field "message" newMessageDataDecoder)
        (D.field "modelDiff" (D.list Diff.diffOperationDecoder))
        (D.oneOf [ D.field "effects" (D.list effectDecoder), D.succeed [] ])


{-| Decoder for subscription change entry data.

The decoder first decompresses the raw JSON value (to handle compressed keys),
then decodes from the decompressed value.

-}
subscriptionChangeDataDecoder : Compression -> D.Decoder SubscriptionChangeData
subscriptionChangeDataDecoder compression =
    D.value
        |> D.andThen
            (\rawValue ->
                let
                    decompressed =
                        CompressionDict.decompressValue compression rawValue
                in
                case D.decodeValue subscriptionChangeDataDecoderInternal decompressed of
                    Ok data ->
                        D.succeed data

                    Err e ->
                        D.fail (D.errorToString e)
            )


{-| Internal decoder for subscription change data (operates on already-decompressed JSON).
-}
subscriptionChangeDataDecoderInternal : D.Decoder SubscriptionChangeData
subscriptionChangeDataDecoderInternal =
    D.map3
        (\ts start stop ->
            { timestamp = ts
            , started = start
            , stopped = stop
            }
        )
        (D.oneOf [ D.field "timestamp" D.int, D.succeed 0 ])
        (D.oneOf [ D.field "started" (D.list D.value), D.succeed [] ])
        (D.oneOf [ D.field "stopped" (D.list D.value), D.succeed [] ])


{-| Decoder for message data in v2 format.

The v2 format uses `_type` for the message type name.
If there's an `_inner` field, that contains the inner message.

Note: Decompression happens at the entry level before this decoder runs.

-}
newMessageDataDecoder : D.Decoder MessageData
newMessageDataDecoder =
    D.andThen
        (\typeStr ->
            D.oneOf
                [ -- Has _inner - use the inner message type
                  D.field "_inner" D.value
                    |> D.andThen (decodeInnerMessage typeStr)
                , -- No _inner - use the whole message as payload
                  D.value
                    |> D.map (\v -> { name = typeStr, payload = v })
                ]
        )
        (D.field "_type" D.string)


{-| Decode inner message, extracting its type name if available.
-}
decodeInnerMessage : String -> D.Value -> D.Decoder MessageData
decodeInnerMessage fallbackType inner =
    case D.decodeValue (D.field "_type" D.string) inner of
        Ok innerType ->
            D.succeed { name = innerType, payload = inner }

        Err _ ->
            D.succeed { name = fallbackType, payload = inner }


{-| Decoder for an effect (command).

Note: Decompression happens at the entry level before this decoder runs.

-}
effectDecoder : D.Decoder Effect
effectDecoder =
    D.oneOf
        [ -- v2 format: _type field identifies the effect type
          D.field "_type" D.string
            |> D.andThen
                (\typeStr ->
                    D.value
                        |> D.map (\v -> { name = typeStr, data = v })
                )
        , -- String-only effect name
          D.string
            |> D.map (\name -> { name = name, data = nullValue })
        ]



-- UTILITIES


{-| A null JSON value.
-}
nullValue : D.Value
nullValue =
    E.null
