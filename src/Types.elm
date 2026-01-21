module Types exposing
    ( LogEntry
    , MessageData
    , Effect
    , ViewMode(..)
    , LoadingState(..)
    , TreePath
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


{-| The current view mode for displaying model state.

  - `PostState`: Shows only the model state after processing the selected message
  - `SplitView`: Shows before and after states side by side
  - `DiffView`: Shows the after state with changed values highlighted

-}
type ViewMode
    = PostState
    | SplitView
    | DiffView { changesOnly : Bool }


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
