module Types exposing
    ( LogEntry
    , MessageData
    , Effect
    , LoadingState(..)
    , TreePath
    , DisplayOrder(..)
    )

{-| Core data types for the TeaForge Debugger application.

This module defines the fundamental types used throughout the application
for representing log entries, messages, effects, and UI state.

-}

import Json.Decode as D


{-| A single log entry from a TeaForge application log file.

Each entry represents one message processed by the TEA update function,
including the state before and after processing, and any effects produced.

-}
type alias LogEntry =
    { timestamp : Int
    , message : MessageData
    , modelBefore : D.Value
    , modelAfter : D.Value
    , effects : List Effect
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


{-| Represents the current loading state of the application.

  - `Idle`: No file has been loaded yet
  - `Loading`: A file is currently being loaded
  - `Loaded`: A file has been successfully loaded
  - `Error`: An error occurred during loading

-}
type LoadingState
    = Idle
    | Loading
    | Loaded
    | Error String


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
