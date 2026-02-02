# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

TeaForge Debugger is an Electron desktop application for debugging TEA (The Elm Architecture) applications. It reads JSONL log files containing state transitions and visualizes them with time-travel debugging capabilities.

## Build Commands

```bash
# Build everything (Elm + CSS)
npm run build

# Build Elm only
npm run build:elm

# Build CSS only (Tailwind)
npm run build:css

# Run the Electron app
npm start

# Development with hot reload
npm run watch:elm    # Elm with elm-live
npm run watch:css    # Tailwind in watch mode

# Run Elm tests
npx elm-test
```

## Browser Automation (electron-agent-tools)

The project uses `electron-agent-tools` for Playwright-based browser automation via CDP (Chrome DevTools Protocol). All commands are available as npm scripts.

### npm Scripts

```bash
# Launch the app
npm run agent:launch

# Launch and wait 3 seconds before returning
npm run agent:launch -- 3

# Quit the app
npm run agent:quit

# Open a log file in the running app
npm run agent:open-file /path/to/file.jsonl

# Take screenshot
npm run agent:screenshot /tmp/screenshot.png

# Click by exact text match
npm run agent:clickOnText "Open File"

# Click by element ID
npm run agent:clickOnId "btn-open-file"

# Type into focused element
npm run agent:type "search text"

# Wait for text to appear
npm run agent:wait-text "Messages"

# Dump DOM
npm run agent:dump-dom

# List interactive elements
npm run agent:list-selectors 20

# List windows
npm run agent:list-windows

# Toggle MCP border visibility (for clean screenshots)
npm run agent:mcp-border hide    # Hide the red border
npm run agent:mcp-border show    # Show the red border
npm run agent:mcp-border toggle  # Toggle visibility (default)
```

The browser-tools scripts automatically read the WebSocket URL from the most recent launch session, so no manual `WS_URL` configuration is needed.

### WebSocket Testing

The app supports connecting to a WebSocket server to receive JSONL entries in real-time. For testing:

```bash
# Start a test WebSocket server (streams new content appended to the log file)
npm run ws:test-server
```

Then connect from the app using the "WebSocket" button and URL `ws://localhost:8080`.

The WebSocket server only sends each message in the file once so you will need to stop and restart the server each time you launch the app to get a fresh set of messages.

### App Flags

The app supports these command-line flags:
- `--mcp-controlled` - Indicates the app is being controlled remotely (enables "agent mode" UI)
- `--open-file=<path>` - Auto-opens the specified log file on startup
- `--remote-debugging-port=<port>` - Enables CDP on the specified port (required for browser-tools)

## Architecture

### Technology Stack
- **Frontend**: Elm 0.19.1 compiled to JS
- **Desktop**: Electron with secure IPC
- **Styling**: Tailwind CSS 4 + DaisyUI 5 (configured in `src/styles.css` via `@plugin`)
- **Icons**: FontAwesome 7 (installed locally via npm as `@fortawesome/fontawesome-free`)

### UI Components

**DaisyUI**: Always prefer DaisyUI components when available. DaisyUI provides pre-styled components built on Tailwind CSS that maintain consistent theming. Common components used:
- `alert` - For error/warning/info messages
- `toast` - For floating notifications (use with `toast-top toast-center` for centered top positioning)
- `btn` - For buttons
- `collapse` - For expandable sections

See https://daisyui.com/components/ for the full component list.

**FontAwesome**: Use FontAwesome icons via the `<i>` element with appropriate classes:
- Solid icons: `fa-solid fa-icon-name` (e.g., `fa-solid fa-triangle-exclamation`)
- Regular icons: `fa-regular fa-icon-name`
- Brand icons: `fa-brands fa-icon-name`

In Elm, render icons as: `i [ class "fa-solid fa-icon-name" ] []`

**Styling**: Always use Tailwind utility classes or DaisyUI component classes for styling. Avoid inline styles (`style "property" "value"` in Elm) except as a last resort when no Tailwind/DaisyUI equivalent exists. This keeps styling consistent and maintainable.

### Elm Module Structure

```
src/
├── Main.elm         # TEA entry point, subscriptions, port handlers
├── Types.elm        # Core types: LogEntry, MessageData, Effect, ViewMode
├── LogParser.elm    # JSONL log file parsing, handles multiple formats
├── TreeView.elm     # JSON tree visualization component
├── Diff.elm         # State diffing between before/after
├── Search.elm       # Search through state tree
├── MessageList.elm  # Sidebar message list component
└── Ports.elm        # Elm-JS port definitions (file ops, WebSocket control)
```

### Electron Process Architecture
- `main.js` - Main process: window management, IPC handlers, native file dialogs
- `preload.js` - Context bridge exposing secure APIs to renderer
- `index.html` - Renderer: loads Elm app, wires ports

### Data Flow

**File-based flow:**
1. User opens file via menu (Cmd+O) or button
2. Electron main process reads file, sends content via IPC
3. JS bridges to Elm via `incoming` port
4. `LogParser` decodes JSONL into `List LogEntry`
5. User selects entry to view state before/after/diff

**WebSocket flow:**
1. User clicks "WebSocket" button and enters URL
2. JS creates WebSocket connection to the server
3. Each message received is parsed as JSON and sent to Elm via `incoming` port
4. Entries are appended incrementally to the log
5. User can view entries as they arrive in real-time

### Log File Format (JSONL)

Each line is a JSON object with entry types: `header`, `init`, `update`, `subscriptionChange`. Key fields in update entries:
- `message` - The dispatched message with `name` and `payload`
- `modelBefore`/`modelAfter` - State snapshots
- `effects` - Commands produced by update

See `log-format.md` for full specification.

### View Options
The state viewer uses two checkboxes to control the display:
- **Show previous state** (default: on) - Shows before/after states side-by-side
- **Highlight changes** (default: on) - Highlights changed values with color coding on the after panel

## Test Data

Test log files for manual testing are located in the `test-data/` directory.

## Elm Conventions

- Uses `elm-json-decode-pipeline` for decoder composition
- Tree state managed separately for before/after panels
- Search results stored as `TreePath` (list of string keys)
- Filter mode shows only paths matching search query

## Available Bash Tools

The following CLI tools are available via bash and should be used when appropriate:

- **jq** - JSON processor for parsing, filtering, and transforming JSON data
- **rg** (ripgrep) - Fast regex-based file content search
- **fd** - Fast file finder (alternative to `find`)
