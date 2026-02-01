#!/usr/bin/env node
/**
 * Opens a file in the running TeaForge Debugger application.
 *
 * Usage: node scripts/open-file.js <file-path>
 *
 * Sends an openInput message to trigger streaming file loading via the main process.
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

    // Connect to browser and send openInput message to Elm
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

        // Send openInput message to Elm to trigger streaming file load
        await page.evaluate(({ filePath }) => {
            if (typeof window.sendToElm === 'function') {
                window.sendToElm({
                    type: 'openInput',
                    payload: {
                        path: filePath
                    }
                });
            } else {
                throw new Error('sendToElm not found on window');
            }
        }, { filePath: absolutePath });

        console.log(JSON.stringify({
            ok: true,
            data: { path: absolutePath, size: stats.size }
        }));

    } catch (err) {
        console.error(JSON.stringify({
            ok: false,
            error: { message: err.message }
        }));
        process.exit(1);
    } finally {
        if (browser) {
            // Disconnect without closing the browser
            await browser.close();
        }
    }
}

main();
