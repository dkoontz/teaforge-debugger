module Types exposing
    ( LogEntry(..)
    , InitEntryData
    , UpdateEntryData
    , SubscriptionChangeData
    , ErrorEntryData
    , MessageData
    , Effect
    , InputSource
    , TreePath
    , DisplayOrder(..)
    , getTimestamp
    , getMessageName
    )

{-| Core data types for the TeaForge Debugger application.

This module defines the fundamental types used throughout the application
for representing log entries, messages, effects, and UI state.

-}

import Json.Decode as D


{-| A single log entry from a TeaForge application log file.

Each entry is one of:

  - `InitEntry` - Initial model and bootstrap effects from init()
  - `UpdateEntry` - Message processing result with before/after model
  - `SubscriptionChangeEntry` - Subscription lifecycle events
  - `ErrorEntry` - A malformed entry that couldn't be parsed

-}
type LogEntry
    = InitEntry InitEntryData
    | UpdateEntry UpdateEntryData
    | SubscriptionChangeEntry SubscriptionChangeData
    | ErrorEntry ErrorEntryData


{-| Data for an init entry.
-}
type alias InitEntryData =
    { timestamp : Int
    , model : D.Value
    , effects : List Effect
    }


{-| Data for an update entry.
-}
type alias UpdateEntryData =
    { timestamp : Int
    , message : MessageData
    , modelBefore : D.Value
    , modelAfter : D.Value
    , effects : List Effect
    }


{-| Data for a subscription change entry.
-}
type alias SubscriptionChangeData =
    { timestamp : Int
    , started : List D.Value
    , stopped : List D.Value
    }


{-| Data for an error entry (malformed line).
-}
type alias ErrorEntryData =
    { lineNumber : Int
    , rawText : String
    , error : String
    }


{-| Represents a message that was dispatched in the TeaForge application.

Contains the message type name and any associated payload data.

-}
type alias MessageData =
    { name : String
    , payload : D.Value
    }


{-| Represents an effect (command) produced by the update function.

Effects are side effects that the TeaForge application requested,
such as HTTP requests, navigation, or other commands.

-}
type alias Effect =
    { name : String
    , data : D.Value
    }


{-| Represents an active input source for log entries.
-}
type alias InputSource =
    { path : String
    , label : String
    }


{-| A path through a tree structure, used for navigation and search results.

Each string in the list represents a key or index at that level of the tree.
For example, ["user", "profile", "name"] represents the path to access
model.user.profile.name in the state tree.

-}
type alias TreePath =
    List String


{-| Controls how messages are displayed in the list.

  - `Chronological`: Oldest messages at top (index 0 = top of display)
  - `ReverseChronological`: Newest messages at top (index 0 = bottom of display)

This affects how "Next" and "Previous" navigation works - they always
correspond to visual direction (down/up) regardless of temporal order.

-}
type DisplayOrder
    = Chronological
    | ReverseChronological


{-| Get the timestamp from a log entry, if available.
-}
getTimestamp : LogEntry -> Maybe Int
getTimestamp entry =
    case entry of
        InitEntry data ->
            Just data.timestamp

        UpdateEntry data ->
            Just data.timestamp

        SubscriptionChangeEntry data ->
            Just data.timestamp

        ErrorEntry _ ->
            Nothing


{-| Get a display name for the entry (message name or entry type).
-}
getMessageName : LogEntry -> String
getMessageName entry =
    case entry of
        InitEntry _ ->
            "Init"

        UpdateEntry data ->
            data.message.name

        SubscriptionChangeEntry _ ->
            "Subscription Change"

        ErrorEntry data ->
            "Parse Error (line " ++ String.fromInt data.lineNumber ++ ")"
