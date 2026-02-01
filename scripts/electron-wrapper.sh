#!/bin/bash
# Wrapper script for electron-agent-tools to launch the TeaForge Debugger
# Provides convenience for launching with CDP enabled and default options
#
# Usage:
#   npm run agent:launch           # Launch with defaults
#   npm run agent:launch -- 3      # Launch and wait 3 seconds
#
# Environment variables:
#   CDP_PORT - Chrome DevTools Protocol port (default: 9333)
#
# To open a file after launch, use:
#   npm run agent:open-file /path/to/file.jsonl

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
APP_DIR="$(dirname "$SCRIPT_DIR")"
ELECTRON_PATH="$APP_DIR/node_modules/electron/dist/Electron.app/Contents/MacOS/Electron"

# Default CDP port
CDP_PORT="${CDP_PORT:-9333}"

# Optional wait time after launch (first argument)
WAIT="${1:-0}"

# Build the args array (no file auto-opened)
ARGS="[\"$APP_DIR\", \"--remote-debugging-port=$CDP_PORT\", \"--mcp-controlled\"]"

# Launch using electron-agent-tools
node "$APP_DIR/node_modules/electron-agent-tools/dist/cli/launch-electron.js" start "{\"command\": \"$ELECTRON_PATH\", \"args\": $ARGS, \"cdpPort\": $CDP_PORT}"

# Optional wait after launch
if [ "$WAIT" -gt 0 ] 2>/dev/null; then
    sleep "$WAIT"
fi
