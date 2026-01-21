const { app, BrowserWindow, ipcMain, dialog } = require('electron');
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

app.whenReady().then(() => {
    createWindow();

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
