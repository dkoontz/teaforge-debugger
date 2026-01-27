const { contextBridge, ipcRenderer } = require('electron');

// Securely expose Electron APIs to the renderer process via contextBridge
// IMPORTANT: Never expose the full ipcRenderer object directly
contextBridge.exposeInMainWorld('electron', {
    // Read file contents from the filesystem
    // @param {string} filePath - Path to the file to read
    // @returns {Promise<{success: boolean, content?: string, error?: string}>}
    readFile: (filePath) => ipcRenderer.invoke('read-file', filePath),

    // List files in a directory
    // @param {string} dirPath - Path to the directory to list
    // @returns {Promise<{success: boolean, files?: Array<{name: string, path: string, isDirectory: boolean, isFile: boolean}>, error?: string}>}
    listFiles: (dirPath) => ipcRenderer.invoke('list-files', dirPath),

    // Open native file dialog to select a TeaForge log file
    // @returns {Promise<{success: boolean, canceled?: boolean, filePath?: string, content?: string, error?: string}>}
    openFileDialog: () => ipcRenderer.invoke('open-file-dialog'),

    // Listen for file-opened events from main process menu (File > Open / Cmd+O)
    // @param {function} callback - Callback to receive file data {filePath: string, content: string}
    onFileOpened: (callback) => {
        ipcRenderer.on('file-opened', (event, data) => callback(data));
    },

    // Check if the app was launched via MCP (remote-controlled)
    // @returns {Promise<boolean>}
    isMcpControlled: () => ipcRenderer.invoke('is-mcp-controlled')
});
