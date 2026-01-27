const { app, BrowserWindow, ipcMain, dialog, Menu } = require('electron');
const path = require('path');
const fs = require('fs');

let mainWindow;

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

// IPC Handler: Read file contents
ipcMain.handle('read-file', async (event, filePath) => {
    try {
        const absolutePath = path.resolve(filePath);
        const content = fs.readFileSync(absolutePath, 'utf8');
        return { success: true, content };
    } catch (error) {
        return { success: false, error: error.message };
    }
});

// IPC Handler: List files in directory
ipcMain.handle('list-files', async (event, dirPath) => {
    try {
        const absolutePath = path.resolve(dirPath);
        const entries = fs.readdirSync(absolutePath, { withFileTypes: true });
        const files = entries.map(entry => ({
            name: entry.name,
            path: path.join(absolutePath, entry.name),
            isDirectory: entry.isDirectory(),
            isFile: entry.isFile()
        }));
        return { success: true, files };
    } catch (error) {
        return { success: false, error: error.message };
    }
});

// IPC Handler: Open file dialog
ipcMain.handle('open-file-dialog', async () => {
    try {
        const result = await dialog.showOpenDialog(mainWindow, {
            properties: ['openFile'],
            filters: [
                { name: 'TeaForge Logs', extensions: ['log', 'json'] },
                { name: 'All Files', extensions: ['*'] }
            ]
        });

        if (result.canceled) {
            return { success: true, canceled: true, filePath: null };
        }

        const filePath = result.filePaths[0];
        const content = fs.readFileSync(filePath, 'utf8');
        return { success: true, canceled: false, filePath, content };
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
                                    { name: 'TeaForge Logs', extensions: ['log', 'json'] },
                                    { name: 'All Files', extensions: ['*'] }
                                ]
                            });

                            if (!result.canceled && result.filePaths.length > 0) {
                                const filePath = result.filePaths[0];
                                try {
                                    const content = fs.readFileSync(filePath, 'utf8');
                                    mainWindow.webContents.send('file-opened', { filePath, content });
                                } catch (error) {
                                    dialog.showErrorBox('Error', `Failed to open file: ${error.message}`);
                                }
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

// IPC Handler: Check if app is MCP-controlled
ipcMain.handle('is-mcp-controlled', () => isMcpControlled);

app.whenReady().then(() => {
    createMenu();
    createWindow();

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
                try {
                    const filePath = path.resolve(autoOpenFile);
                    const content = fs.readFileSync(filePath, 'utf8');
                    mainWindow.webContents.send('file-opened', { filePath, content });
                } catch (error) {
                    console.error(`Failed to auto-open file: ${error.message}`);
                }
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
