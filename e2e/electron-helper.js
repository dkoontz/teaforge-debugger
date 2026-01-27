/**
 * Electron E2E Helper
 *
 * Provides utilities for launching the Electron app, taking screenshots,
 * and interacting with the UI via Playwright.
 *
 * Usage:
 *   node e2e/electron-helper.js screenshot [filename]
 *   node e2e/electron-helper.js screenshot-with-file <log-file-path> [filename]
 */

const { _electron: electron } = require('@playwright/test');
const path = require('path');
const fs = require('fs');

const SCREENSHOTS_DIR = path.join(__dirname, 'temp');

// Ensure screenshots directory exists
if (!fs.existsSync(SCREENSHOTS_DIR)) {
  fs.mkdirSync(SCREENSHOTS_DIR, { recursive: true });
}

/**
 * Launch the Electron app and return the app and window objects
 */
async function launchApp() {
  const electronApp = await electron.launch({
    args: [path.join(__dirname, '..', 'main.js')],
    cwd: path.join(__dirname, '..'),
  });

  // Wait for the first window to open
  const window = await electronApp.firstWindow();

  // Wait for the app to be ready (Elm app initialized)
  await window.waitForLoadState('domcontentloaded');
  await window.waitForTimeout(500); // Give Elm time to render

  return { electronApp, window };
}

/**
 * Take a screenshot of the current window state
 */
async function takeScreenshot(window, filename = 'screenshot.png') {
  const screenshotPath = path.join(SCREENSHOTS_DIR, filename);
  await window.screenshot({ path: screenshotPath });
  console.log(`Screenshot saved to: ${screenshotPath}`);
  return screenshotPath;
}

/**
 * Open a log file in the app
 */
async function openLogFile(window, filePath) {
  // Use the Electron IPC to simulate file open
  // The app listens for file-opened events from main process
  const absolutePath = path.resolve(filePath);

  if (!fs.existsSync(absolutePath)) {
    throw new Error(`File not found: ${absolutePath}`);
  }

  const content = fs.readFileSync(absolutePath, 'utf-8');

  // Evaluate in the renderer to send the file content to Elm
  await window.evaluate(({ filePath, content }) => {
    // Access the Elm app's incoming port via the global sendToElm function
    if (window.sendToElm) {
      window.sendToElm({
        type: 'fileReadResult',
        payload: {
          success: true,
          content: content,
          path: filePath
        }
      });
    } else if (window.app && window.app.ports && window.app.ports.incoming) {
      window.app.ports.incoming.send({
        type: 'fileReadResult',
        payload: {
          success: true,
          content: content,
          path: filePath
        }
      });
    }
  }, { filePath: absolutePath, content });

  // Wait for the app to process the file
  await window.waitForTimeout(500);
}

/**
 * Click on a message in the sidebar by index (0-based, from top of displayed list)
 */
async function selectMessage(window, index) {
  const selector = `#message-item-${index}`;
  await window.click(selector);
  await window.waitForTimeout(200);
}

/**
 * Press arrow key to navigate messages
 */
async function navigateMessages(window, direction = 'down') {
  const key = direction === 'down' ? 'ArrowDown' : 'ArrowUp';
  await window.keyboard.press(key);
  await window.waitForTimeout(200);
}

// CLI interface
async function main() {
  const args = process.argv.slice(2);
  const command = args[0];

  if (!command) {
    console.log(`
Electron E2E Helper

Commands:
  screenshot [filename]                    Take a screenshot of the app
  screenshot-with-file <file> [filename]   Open a log file and take a screenshot
  interactive                              Launch app and keep it open for manual testing

Examples:
  node e2e/electron-helper.js screenshot app-empty.png
  node e2e/electron-helper.js screenshot-with-file test-data/sample.jsonl app-loaded.png
    `);
    process.exit(0);
  }

  const { electronApp, window } = await launchApp();

  try {
    switch (command) {
      case 'screenshot': {
        const filename = args[1] || 'screenshot.png';
        await takeScreenshot(window, filename);
        break;
      }

      case 'screenshot-with-file': {
        const filePath = args[1];
        const filename = args[2] || 'screenshot.png';

        if (!filePath) {
          console.error('Error: Please provide a log file path');
          process.exit(1);
        }

        await openLogFile(window, filePath);
        await takeScreenshot(window, filename);
        break;
      }

      case 'interactive': {
        console.log('App launched in interactive mode. Press Ctrl+C to exit.');
        // Keep the app open
        await new Promise(() => {});
        break;
      }

      default:
        console.error(`Unknown command: ${command}`);
        process.exit(1);
    }
  } finally {
    if (command !== 'interactive') {
      await electronApp.close();
    }
  }
}

// Export functions for use in tests
module.exports = {
  launchApp,
  takeScreenshot,
  openLogFile,
  selectMessage,
  navigateMessages,
  SCREENSHOTS_DIR,
};

// Run CLI if executed directly
if (require.main === module) {
  main().catch(err => {
    console.error(err);
    process.exit(1);
  });
}
