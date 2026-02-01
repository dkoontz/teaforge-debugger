port module Ports exposing
    ( incoming
    , outgoing
    , openFileDialog
    , openInput
    , scrollIntoView
    , focusElement
    , saveSidebarWidth
    )

{-| Port module for JavaScript interop.

This module defines a single port pair for communication between Elm and JavaScript.
All messages are sent as JSON Values with a type discriminator for simpler architecture.

## Outgoing Commands (Elm -> JavaScript)

  - `openFileDialog`: Request native file dialog
  - `openInput`: Open an input source for streaming entries

## Incoming Responses (JavaScript -> Elm)

  - `openInput`: Signal to open an input source (from dialog or menu)
  - `inputOpened`: Result of opening an input source
  - `entryReceived`: A log entry was received
  - `inputError`: An error occurred with the input source
  - `inputClosed`: The input source stream finished

-}

import Json.Encode as E


{-| Port for sending commands from Elm to JavaScript.

All messages should have a `type` field and an optional `payload` field.

-}
port outgoing : E.Value -> Cmd msg


{-| Port for receiving responses from JavaScript to Elm.

All messages will have a `type` field and a `payload` field.

-}
port incoming : (E.Value -> msg) -> Sub msg



-- OUTGOING COMMANDS


{-| Request the native file dialog to open a TeaForge log file.

This now triggers the streaming flow: after dialog selection,
JS will send an `openInput` message back to Elm.

Sends: `{ type: "openFileDialog", payload: null }`

-}
openFileDialog : Cmd msg
openFileDialog =
    outgoing
        (E.object
            [ ( "type", E.string "openFileDialog" )
            , ( "payload", E.null )
            ]
        )


{-| Request to open an input source for streaming log entries.

Sends: `{ type: "openInput", payload: { path: string } }`
Expects: `{ type: "inputOpened", payload: { success: bool, path: string, error: string } }`
Then receives: `{ type: "entryReceived", payload: { lineNumber: int, entry?: value, error?: string, rawText?: string } }`

-}
openInput : String -> Cmd msg
openInput path =
    outgoing
        (E.object
            [ ( "type", E.string "openInput" )
            , ( "payload"
              , E.object
                    [ ( "path", E.string path )
                    ]
              )
            ]
        )


{-| Request to scroll an element into view (only scrolls if element is out of view).

Sends: `{ type: "scrollIntoView", payload: { elementId: string } }`
No response expected.

-}
scrollIntoView : String -> Cmd msg
scrollIntoView elementId =
    outgoing
        (E.object
            [ ( "type", E.string "scrollIntoView" )
            , ( "payload"
              , E.object
                    [ ( "elementId", E.string elementId )
                    ]
              )
            ]
        )


{-| Request to focus an element by ID.

Sends: `{ type: "focusElement", payload: { elementId: string } }`
No response expected.

-}
focusElement : String -> Cmd msg
focusElement elementId =
    outgoing
        (E.object
            [ ( "type", E.string "focusElement" )
            , ( "payload"
              , E.object
                    [ ( "elementId", E.string elementId )
                    ]
              )
            ]
        )


{-| Save the sidebar width to localStorage.

Sends: `{ type: "saveSidebarWidth", payload: { width: int } }`
No response expected.

-}
saveSidebarWidth : Int -> Cmd msg
saveSidebarWidth width =
    outgoing
        (E.object
            [ ( "type", E.string "saveSidebarWidth" )
            , ( "payload"
              , E.object
                    [ ( "width", E.int width )
                    ]
              )
            ]
        )
