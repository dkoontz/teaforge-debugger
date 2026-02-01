#!/usr/bin/env node
/**
 * Opens a file in the running TeaForge Debugger application.
 *
 * Usage: node scripts/open-file.js <file-path>
 *
 * Reads the file content and sends it to the Elm app via the sendToElm function
 * exposed on the window object.
 */

const fs = require('fs');
const path = require('path');
const { chromium } = require('playwright');

async function main() {
    const filePath = process.argv[2];

    if (!filePath) {
        console.error(JSON.stringify({
            ok: false,
            error: { message: 'Usage: node scripts/open-file.js <file-path>' }
        }));
        process.exit(1);
    }

    // Resolve to absolute path
    const absolutePath = path.resolve(filePath);

    // Check file exists and get stats
    let stats;
    try {
        stats = fs.statSync(absolutePath);
    } catch (err) {
        console.error(JSON.stringify({
            ok: false,
            error: { message: `File not found: ${absolutePath}` }
        }));
        process.exit(1);
    }

    // Check file size - V8 has a ~512MB string limit, we'll use 500MB as our limit
    const MAX_FILE_SIZE = 500 * 1024 * 1024; // 500 MB
    const fileSizeMB = (stats.size / (1024 * 1024)).toFixed(1);
    if (stats.size > MAX_FILE_SIZE) {
        console.error(JSON.stringify({
            ok: false,
            error: {
                message: `File too large: ${fileSizeMB} MB exceeds the 500 MB limit. Consider splitting the log file into smaller chunks.`,
                code: 'FILE_TOO_LARGE',
                size: stats.size
            }
        }));
        process.exit(1);
    }

    // Read file content
    let content;
    try {
        content = fs.readFileSync(absolutePath, 'utf-8');
    } catch (err) {
        // Check for V8 string length error
        if (err.message && err.message.includes('string longer than')) {
            console.error(JSON.stringify({
                ok: false,
                error: {
                    message: `File too large: ${fileSizeMB} MB exceeds JavaScript's string size limit. Consider splitting the log file into smaller chunks.`,
                    code: 'FILE_TOO_LARGE',
                    size: stats.size
                }
            }));
        } else {
            console.error(JSON.stringify({
                ok: false,
                error: { message: `Failed to read file: ${err.message}` }
            }));
        }
        process.exit(1);
    }

    // Find WS_URL from environment or launch.json
    let wsUrl = process.env.WS_URL;
    if (!wsUrl) {
        const appDir = path.join(__dirname, '..');
        const artifactsDir = path.join(appDir, '.e2e-artifacts');

        try {
            const dirs = fs.readdirSync(artifactsDir)
                .map(d => ({ name: d, path: path.join(artifactsDir, d) }))
                .filter(d => fs.statSync(d.path).isDirectory())
                .sort((a, b) => b.name.localeCompare(a.name));

            for (const dir of dirs) {
                const launchFile = path.join(dir.path, 'launch.json');
                if (fs.existsSync(launchFile)) {
                    const launch = JSON.parse(fs.readFileSync(launchFile, 'utf-8'));
                    wsUrl = launch.wsUrl;
                    break;
                }
            }
        } catch (err) {
            // Ignore errors reading artifacts
        }
    }

    if (!wsUrl) {
        console.error(JSON.stringify({
            ok: false,
            error: { message: 'No running app found. Run "npm run agent:launch" first.' }
        }));
        process.exit(1);
    }

    // Connect to browser and send file to Elm
    let browser;
    try {
        browser = await chromium.connectOverCDP(wsUrl);
        const contexts = browser.contexts();

        if (contexts.length === 0) {
            throw new Error('No browser contexts found');
        }

        const pages = contexts[0].pages();
        if (pages.length === 0) {
            throw new Error('No pages found');
        }

        const page = pages[0];

        // Send file content to Elm via sendToElm
        await page.evaluate(({ content, filePath }) => {
            if (typeof window.sendToElm === 'function') {
                window.sendToElm({
                    type: 'fileReadResult',
                    payload: {
                        success: true,
                        content: content,
                        path: filePath
                    }
                });
            } else {
                throw new Error('sendToElm not found on window');
            }
        }, { content, filePath: absolutePath });

        console.log(JSON.stringify({
            ok: true,
            data: { path: absolutePath, size: content.length }
        }));

    } catch (err) {
        // Check for V8 string length error (can happen during IPC/evaluate)
        const fileSizeMB = (content.length / (1024 * 1024)).toFixed(1);
        if (err.message && err.message.includes('string longer than')) {
            console.error(JSON.stringify({
                ok: false,
                error: {
                    message: `File too large: ${fileSizeMB} MB exceeds JavaScript's string size limit. Consider splitting the log file into smaller chunks.`,
                    code: 'FILE_TOO_LARGE',
                    size: content.length
                }
            }));
        } else {
            console.error(JSON.stringify({
                ok: false,
                error: { message: err.message }
            }));
        }
        process.exit(1);
    } finally {
        if (browser) {
            // Disconnect without closing the browser
            await browser.close();
        }
    }
}

main();
