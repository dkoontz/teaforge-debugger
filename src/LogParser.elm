module LogParser exposing
    ( entryTypeDecoder
    , initDataDecoder
    , updateDataDecoder
    , subscriptionChangeDataDecoder
    , messageDataDecoder
    , effectDecoder
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

    messageDataDecoder : D.Decoder MessageData
    effectDecoder : D.Decoder Effect

-}

import Json.Decode as D
import Json.Encode as E
import Types exposing (Effect, MessageData)


{-| Intermediate type for decoded init entry data.
-}
type alias InitData =
    { timestamp : Int
    , model : D.Value
    , effects : List Effect
    }


{-| Intermediate type for decoded update entry data.
-}
type alias UpdateData =
    { timestamp : Int
    , message : MessageData
    , model : D.Value
    , effects : List Effect
    }


{-| Intermediate type for decoded subscription change entry data.
-}
type alias SubscriptionChangeData =
    { timestamp : Int
    , started : List D.Value
    , stopped : List D.Value
    }


{-| Decoder for the entry type field.
-}
entryTypeDecoder : D.Decoder String
entryTypeDecoder =
    D.field "type" D.string


{-| Decoder for init entry data.
-}
initDataDecoder : D.Decoder InitData
initDataDecoder =
    D.map3
        (\ts model effs ->
            { timestamp = ts
            , model = model
            , effects = effs
            }
        )
        (D.oneOf [ D.field "timestamp" D.int, D.succeed 0 ])
        (D.field "model" D.value)
        (D.oneOf [ D.field "effects" (D.list effectDecoder), D.succeed [] ])


{-| Decoder for update entry data.
-}
updateDataDecoder : D.Decoder UpdateData
updateDataDecoder =
    D.map4
        (\ts msg model effs ->
            { timestamp = ts
            , message = msg
            , model = model
            , effects = effs
            }
        )
        (D.oneOf [ D.field "timestamp" D.int, D.succeed 0 ])
        (D.field "message" newMessageDataDecoder)
        (D.field "model" D.value)
        (D.oneOf [ D.field "effects" (D.list effectDecoder), D.succeed [] ])


{-| Decoder for subscription change entry data.
-}
subscriptionChangeDataDecoder : D.Decoder SubscriptionChangeData
subscriptionChangeDataDecoder =
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



-- UTILITIES


{-| A null JSON value.
-}
nullValue : D.Value
nullValue =
    E.null
