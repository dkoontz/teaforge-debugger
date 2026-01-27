port module Ports exposing
    ( incoming
    , outgoing
    , openFileDialog
    , readFile
    , listFiles
    , scrollIntoView
    , focusElement
    , saveSidebarWidth
    )

{-| Port module for JavaScript interop.

This module defines a single port pair for communication between Elm and JavaScript.
All messages are sent as JSON Values with a type discriminator for simpler architecture.

## Outgoing Commands (Elm -> JavaScript)

  - `openFileDialog`: Request native file dialog
  - `readFile`: Read contents of a file
  - `listFiles`: List files in a directory

## Incoming Responses (JavaScript -> Elm)

  - `fileDialogResult`: Result of file dialog
  - `fileReadResult`: Contents of a file
  - `fileListResult`: List of files
  - `error`: Error response

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

Sends: `{ type: "openFileDialog", payload: null }`
Expects: `{ type: "fileDialogResult", payload: { success: bool, filePath: string, error: string } }`

-}
openFileDialog : Cmd msg
openFileDialog =
    outgoing
        (E.object
            [ ( "type", E.string "openFileDialog" )
            , ( "payload", E.null )
            ]
        )


{-| Request to read the contents of a file at the given path.

Sends: `{ type: "readFile", payload: { path: string } }`
Expects: `{ type: "fileReadResult", payload: { success: bool, content: string, path: string, error: string } }`

-}
readFile : String -> Cmd msg
readFile path =
    outgoing
        (E.object
            [ ( "type", E.string "readFile" )
            , ( "payload"
              , E.object
                    [ ( "path", E.string path )
                    ]
              )
            ]
        )


{-| Request to list files in a directory.

Sends: `{ type: "listFiles", payload: { path: string } }`
Expects: `{ type: "fileListResult", payload: { success: bool, files: [string], path: string, error: string } }`

-}
listFiles : String -> Cmd msg
listFiles path =
    outgoing
        (E.object
            [ ( "type", E.string "listFiles" )
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
