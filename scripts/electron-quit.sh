#!/bin/bash
# Wrapper script to quit the Electron app
# Reads LAUNCH_FILE from environment or uses most recent artifact
#
# Usage:
#   LAUNCH_FILE=.e2e-artifacts/xxx/launch.json ./scripts/electron-quit.sh
#   ./scripts/electron-quit.sh  # Uses most recent launch.json

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
APP_DIR="$(dirname "$SCRIPT_DIR")"
LAUNCH_ELECTRON="$APP_DIR/node_modules/electron-agent-tools/dist/cli/launch-electron.js"

# If LAUNCH_FILE not set, find the most recent one
if [ -z "$LAUNCH_FILE" ]; then
    LAUNCH_FILE=$(ls -td "$APP_DIR"/.e2e-artifacts/*/launch.json 2>/dev/null | head -1)
    if [ -z "$LAUNCH_FILE" ]; then
        echo "Error: No launch.json found. Set LAUNCH_FILE or run agent:launch first." >&2
        exit 1
    fi
fi

exec node "$LAUNCH_ELECTRON" quit "{\"launchFile\": \"$LAUNCH_FILE\"}"
