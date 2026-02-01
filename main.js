const { app, BrowserWindow, ipcMain, dialog, Menu } = require('electron');
const path = require('path');
const fs = require('fs');
const readline = require('readline');

let mainWindow;

// Track active input stream
let activeStream = null;
let lineNumber = 0;

function createWindow() {
    mainWindow = new BrowserWindow({
        width: 1400,
        height: 900,
        minWidth: 800,
        minHeight: 600,
        webPreferences: {
            preload: path.join(__dirname, 'preload.js'),
            contextIsolation: true,
            nodeIntegration: false
        }
    });

    mainWindow.loadFile('index.html');

    mainWindow.on('closed', () => {
        mainWindow = null;
    });
}

// IPC Handler: Open input source (streaming file read)
ipcMain.handle('open-input', async (event, filePath) => {
    // Close any existing stream
    if (activeStream) {
        activeStream.close();
        activeStream = null;
    }

    lineNumber = 0;
    const absolutePath = path.resolve(filePath);

    // Check file exists
    try {
        fs.accessSync(absolutePath, fs.constants.R_OK);
    } catch (err) {
        return { success: false, error: `File not found: ${absolutePath}` };
    }

    try {
        const stream = fs.createReadStream(absolutePath, { encoding: 'utf8' });
        const rl = readline.createInterface({ input: stream });

        activeStream = rl;

        rl.on('line', (line) => {
            lineNumber++;
            const trimmed = line.trim();
            if (!trimmed) return; // skip empty lines

            try {
                const parsed = JSON.parse(trimmed);
                mainWindow.webContents.send('entry-received', {
                    lineNumber,
                    entry: parsed
                });
            } catch (e) {
                // Send parse error as an error entry
                mainWindow.webContents.send('entry-received', {
                    lineNumber,
                    error: e.message,
                    rawText: trimmed.substring(0, 200)
                });
            }
        });

        rl.on('close', () => {
            mainWindow.webContents.send('input-closed', {});
            activeStream = null;
        });

        rl.on('error', (err) => {
            mainWindow.webContents.send('input-error', { error: err.message });
        });

        return { success: true, path: absolutePath };
    } catch (err) {
        return { success: false, error: err.message };
    }
});

// IPC Handler: Close input source
ipcMain.handle('close-input', async () => {
    if (activeStream) {
        activeStream.close();
        activeStream = null;
    }
    return { success: true };
});

// IPC Handler: Open file dialog (returns selected path, doesn't read file)
ipcMain.handle('open-file-dialog', async () => {
    try {
        const result = await dialog.showOpenDialog(mainWindow, {
            properties: ['openFile'],
            filters: [
                { name: 'TeaForge Logs', extensions: ['log', 'jsonl', 'json'] },
                { name: 'All Files', extensions: ['*'] }
            ]
        });

        if (result.canceled) {
            return { success: true, canceled: true, filePath: null };
        }

        return { success: true, canceled: false, filePath: result.filePaths[0] };
    } catch (error) {
        return { success: false, error: error.message };
    }
});

// Create application menu
function createMenu() {
    const isMac = process.platform === 'darwin';

    const template = [
        // App menu (macOS only)
        ...(isMac ? [{
            label: app.name,
            submenu: [
                { role: 'about' },
                { type: 'separator' },
                { role: 'services' },
                { type: 'separator' },
                { role: 'hide' },
                { role: 'hideOthers' },
                { role: 'unhide' },
                { type: 'separator' },
                { role: 'quit' }
            ]
        }] : []),
        // File menu
        {
            label: 'File',
            submenu: [
                {
                    label: 'Open...',
                    accelerator: 'CmdOrCtrl+O',
                    click: async () => {
                        if (mainWindow) {
                            const result = await dialog.showOpenDialog(mainWindow, {
                                properties: ['openFile'],
                                filters: [
                                    { name: 'TeaForge Logs', extensions: ['log', 'json', 'jsonl'] },
                                    { name: 'All Files', extensions: ['*'] }
                                ]
                            });

                            if (!result.canceled && result.filePaths.length > 0) {
                                const filePath = result.filePaths[0];
                                // Send the file path to open via streaming
                                mainWindow.webContents.send('file-selected', { filePath });
                            }
                        }
                    }
                },
                { type: 'separator' },
                isMac ? { role: 'close' } : { role: 'quit' }
            ]
        },
        // Edit menu
        {
            label: 'Edit',
            submenu: [
                { role: 'undo' },
                { role: 'redo' },
                { type: 'separator' },
                { role: 'cut' },
                { role: 'copy' },
                { role: 'paste' },
                { role: 'selectAll' }
            ]
        },
        // View menu
        {
            label: 'View',
            submenu: [
                { role: 'reload' },
                { role: 'forceReload' },
                { role: 'toggleDevTools' },
                { type: 'separator' },
                { role: 'resetZoom' },
                { role: 'zoomIn' },
                { role: 'zoomOut' },
                { type: 'separator' },
                { role: 'togglefullscreen' }
            ]
        },
        // Window menu
        {
            label: 'Window',
            submenu: [
                { role: 'minimize' },
                { role: 'zoom' },
                ...(isMac ? [
                    { type: 'separator' },
                    { role: 'front' }
                ] : [
                    { role: 'close' }
                ])
            ]
        }
    ];

    const menu = Menu.buildFromTemplate(template);
    Menu.setApplicationMenu(menu);
}

// Handle --test-launch flag for verification
const isTestLaunch = process.argv.includes('--test-launch');

// Handle --mcp-controlled flag to indicate remote control via MCP
const isMcpControlled = process.argv.includes('--mcp-controlled');

// Handle --open-file=<path> argument to auto-open a file on startup
const openFileArg = process.argv.find(arg => arg.startsWith('--open-file='));
const autoOpenFile = openFileArg ? openFileArg.substring('--open-file='.length) : null;

// Handle --dev-tools flag to open DevTools on startup
const openDevTools = process.argv.includes('--dev-tools');

// IPC Handler: Check if app is MCP-controlled
ipcMain.handle('is-mcp-controlled', () => isMcpControlled);

app.whenReady().then(() => {
    createMenu();
    createWindow();

    // Open DevTools if --dev-tools flag is present
    if (openDevTools) {
        mainWindow.webContents.openDevTools();
    }

    // If running in test mode, quit after window is ready
    if (isTestLaunch) {
        mainWindow.webContents.on('did-finish-load', () => {
            // Small delay to ensure Elm app initializes
            setTimeout(() => {
                app.quit();
            }, 1000);
        });
    }

    // Auto-open file if specified via --open-file argument
    if (autoOpenFile) {
        mainWindow.webContents.on('did-finish-load', () => {
            // Small delay to ensure Elm app initializes
            setTimeout(() => {
                const filePath = path.resolve(autoOpenFile);
                // Send file-selected event to trigger streaming open
                mainWindow.webContents.send('file-selected', { filePath });
            }, 100);
        });
    }

    app.on('activate', () => {
        if (BrowserWindow.getAllWindows().length === 0) {
            createWindow();
        }
    });
});

app.on('window-all-closed', () => {
    if (process.platform !== 'darwin') {
        app.quit();
    }
});
