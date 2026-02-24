#!/bin/bash
# Wrapper script for electron-agent-tools browser-tools
# Automatically reads WS_URL from the most recent launch.json, or from environment
#
# Usage:
#   ./scripts/browser-tools.sh screenshot /tmp/screenshot.png
#   ./scripts/browser-tools.sh click "Button Text"
#   ./scripts/browser-tools.sh type "search text"
#   ./scripts/browser-tools.sh wait-text "Expected Text"
#   ./scripts/browser-tools.sh dump-dom
#   ./scripts/browser-tools.sh list-selectors [max]
#   ./scripts/browser-tools.sh list-windows
#   ./scripts/browser-tools.sh open-file /path/to/file.jsonl
#   ./scripts/browser-tools.sh mcp-border [show|hide|toggle]

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
APP_DIR="$(dirname "$SCRIPT_DIR")"
BROWSER_TOOLS="$APP_DIR/node_modules/electron-agent-tools/dist/cli/browser-tools.js"

# If WS_URL not set, read from most recent launch.json
if [ -z "$WS_URL" ]; then
    LAUNCH_FILE=$(ls -td "$APP_DIR"/.e2e-artifacts/*/launch.json 2>/dev/null | head -1)
    if [ -z "$LAUNCH_FILE" ]; then
        echo "Error: No launch.json found. Run 'npm run agent:launch' first." >&2
        exit 1
    fi
    WS_URL=$(jq -r '.wsUrl' "$LAUNCH_FILE")
    if [ -z "$WS_URL" ] || [ "$WS_URL" = "null" ]; then
        echo "Error: Could not read wsUrl from $LAUNCH_FILE" >&2
        exit 1
    fi
fi

COMMAND="$1"
shift

case "$COMMAND" in
    screenshot)
        PATH_ARG="${1:-/tmp/screenshot.png}"
        node "$BROWSER_TOOLS" screenshot "{\"wsUrl\": \"$WS_URL\", \"path\": \"$PATH_ARG\"}"
        STATUS=$?
        if [ $STATUS -eq 0 ] && [ -f "$PATH_ARG" ]; then
            sips -Z 1024 "$PATH_ARG" --out "$PATH_ARG" > /dev/null 2>&1
        fi
        exit $STATUS
        ;;
    click-on-text)
        TEXT_ARG="$1"
        if [ -z "$TEXT_ARG" ]; then
            echo "Error: click-on-text requires text argument" >&2
            exit 1
        fi
        # Custom script needed for exact text matching (library hardcodes exact: false)
        exec node "$SCRIPT_DIR/click-on-text.js" "$TEXT_ARG"
        ;;
    click-on-id)
        ID_ARG="$1"
        if [ -z "$ID_ARG" ]; then
            echo "Error: click-on-id requires element ID argument" >&2
            exit 1
        fi
        # Use CSS selector for ID - supported by the library
        exec node "$BROWSER_TOOLS" click "{\"wsUrl\": \"$WS_URL\", \"css\": \"#$ID_ARG\"}"
        ;;
    type)
        VALUE_ARG="$1"
        if [ -z "$VALUE_ARG" ]; then
            echo "Error: type requires value argument" >&2
            exit 1
        fi
        exec node "$BROWSER_TOOLS" type "{\"wsUrl\": \"$WS_URL\", \"value\": \"$VALUE_ARG\"}"
        ;;
    wait-text)
        TEXT_ARG="$1"
        if [ -z "$TEXT_ARG" ]; then
            echo "Error: wait-text requires text argument" >&2
            exit 1
        fi
        exec node "$BROWSER_TOOLS" wait-text "{\"wsUrl\": \"$WS_URL\", \"text\": \"$TEXT_ARG\"}"
        ;;
    dump-dom)
        exec node "$BROWSER_TOOLS" dump-dom "{\"wsUrl\": \"$WS_URL\"}"
        ;;
    list-selectors)
        MAX_ARG="${1:-20}"
        exec node "$BROWSER_TOOLS" list-selectors "{\"wsUrl\": \"$WS_URL\", \"max\": $MAX_ARG}"
        ;;
    list-windows)
        exec node "$BROWSER_TOOLS" list-windows "{\"wsUrl\": \"$WS_URL\"}"
        ;;
    open-file)
        FILE_ARG="$1"
        if [ -z "$FILE_ARG" ]; then
            echo "Error: open-file requires file path argument" >&2
            exit 1
        fi
        exec node "$SCRIPT_DIR/open-file.js" "$FILE_ARG"
        ;;
    mcp-border)
        ACTION_ARG="${1:-toggle}"
        exec node "$SCRIPT_DIR/toggle-mcp-border.js" "$ACTION_ARG"
        ;;
    *)
        echo "Unknown command: $COMMAND" >&2
        echo "Available commands: screenshot, click-on-text, click-on-id, type, wait-text, dump-dom, list-selectors, list-windows, open-file, mcp-border" >&2
        exit 1
        ;;
esac
