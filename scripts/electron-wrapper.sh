#!/bin/bash
# Wrapper script for electron-agent-tools to launch the TeaForge Debugger
# Provides convenience for launching with CDP enabled and default options
#
# Usage:
#   npm run agent:launch           # Launch with defaults
#   npm run agent:launch -- 3      # Launch and wait 3 seconds
#
# Environment variables:
#   LOG_FILE - Path to log file to open (default: test-data/tie-dye.log)
#   CDP_PORT - Chrome DevTools Protocol port (default: 9333)

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
APP_DIR="$(dirname "$SCRIPT_DIR")"
ELECTRON_PATH="$APP_DIR/node_modules/electron/dist/Electron.app/Contents/MacOS/Electron"

# Default CDP port
CDP_PORT="${CDP_PORT:-9333}"

# Default log file to open (can be overridden via LOG_FILE env var)
LOG_FILE="${LOG_FILE:-$APP_DIR/test-data/tie-dye.log}"

# Optional wait time after launch (first argument)
WAIT="${1:-0}"

# Build the args array
ARGS="[\"$APP_DIR\", \"--remote-debugging-port=$CDP_PORT\", \"--mcp-controlled\""
if [ -n "$LOG_FILE" ]; then
    ARGS="$ARGS, \"--open-file=$LOG_FILE\""
fi
ARGS="$ARGS]"

# Launch using electron-agent-tools
node "$APP_DIR/node_modules/electron-agent-tools/dist/cli/launch-electron.js" start "{\"command\": \"$ELECTRON_PATH\", \"args\": $ARGS, \"cdpPort\": $CDP_PORT}"

# Optional wait after launch
if [ "$WAIT" -gt 0 ] 2>/dev/null; then
    sleep "$WAIT"
fi
