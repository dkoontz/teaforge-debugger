#!/usr/bin/env node
/**
 * Clicks on an element by exact text match.
 *
 * Usage: node scripts/click-on-text.js "Button Text"
 */

const fs = require('fs');
const path = require('path');
const { chromium } = require('playwright');

async function main() {
    const text = process.argv[2];

    if (!text) {
        console.error(JSON.stringify({
            ok: false,
            error: { message: 'Usage: node scripts/click-on-text.js "Button Text"' }
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

        // Use exact text matching
        const locator = page.getByText(text, { exact: true });
        await locator.click({ timeout: 5000 });

        console.log(JSON.stringify({
            ok: true,
            data: { clicked: true, text }
        }));

    } catch (err) {
        console.error(JSON.stringify({
            ok: false,
            error: { message: err.message }
        }));
        process.exit(1);
    } finally {
        if (browser) {
            await browser.close();
        }
    }
}

main();
