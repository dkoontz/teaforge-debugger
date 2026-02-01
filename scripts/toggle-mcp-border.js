#!/usr/bin/env node
/**
 * Toggles the MCP border visibility in the running TeaForge Debugger application.
 *
 * Usage: node scripts/toggle-mcp-border.js [show|hide|toggle]
 *
 * Default action is "toggle" if no argument is provided.
 */

const fs = require('fs');
const path = require('path');
const { chromium } = require('playwright');

async function main() {
    const action = process.argv[2] || 'toggle';

    if (!['show', 'hide', 'toggle'].includes(action)) {
        console.error(JSON.stringify({
            ok: false,
            error: { message: 'Usage: node scripts/toggle-mcp-border.js [show|hide|toggle]' }
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

    // Connect to browser and toggle the MCP border
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

        // Toggle the MCP border visibility
        const result = await page.evaluate((action) => {
            const border = document.getElementById('mcp-border');
            if (!border) {
                return { visible: false, error: 'mcp-border element not found' };
            }

            let visible;
            if (action === 'show') {
                border.classList.remove('hidden');
                visible = true;
            } else if (action === 'hide') {
                border.classList.add('hidden');
                visible = false;
            } else {
                // toggle
                border.classList.toggle('hidden');
                visible = !border.classList.contains('hidden');
            }

            return { visible };
        }, action);

        console.log(JSON.stringify({
            ok: true,
            data: { action, visible: result.visible }
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
