const { contextBridge, ipcRenderer } = require('electron');

// Securely expose Electron APIs to the renderer process via contextBridge
// IMPORTANT: Never expose the full ipcRenderer object directly
contextBridge.exposeInMainWorld('electron', {
    // Open an input source (file) for streaming log entries
    // @param {string} filePath - Path to the file to read
    // @returns {Promise<{success: boolean, path?: string, error?: string}>}
    openInput: (filePath) => ipcRenderer.invoke('open-input', filePath),

    // Close the current input source
    // @returns {Promise<{success: boolean}>}
    closeInput: () => ipcRenderer.invoke('close-input'),

    // Listen for entry-received events (one per log line)
    // @param {function} callback - Callback to receive entry data
    //   {lineNumber: int, entry?: object, error?: string, rawText?: string}
    onEntryReceived: (callback) => {
        ipcRenderer.on('entry-received', (event, data) => callback(data));
    },

    // Listen for input-error events (stream-level errors)
    // @param {function} callback - Callback to receive error data {error: string}
    onInputError: (callback) => {
        ipcRenderer.on('input-error', (event, data) => callback(data));
    },

    // Listen for input-closed events (stream finished)
    // @param {function} callback - Callback when stream closes
    onInputClosed: (callback) => {
        ipcRenderer.on('input-closed', (event, data) => callback(data));
    },

    // Listen for file-selected events from main process menu (File > Open / Cmd+O)
    // @param {function} callback - Callback to receive file path {filePath: string}
    onFileSelected: (callback) => {
        ipcRenderer.on('file-selected', (event, data) => callback(data));
    },

    // Open native file dialog to select a TeaForge log file
    // @returns {Promise<{success: boolean, canceled?: boolean, filePath?: string, error?: string}>}
    openFileDialog: () => ipcRenderer.invoke('open-file-dialog'),

    // Check if the app was launched via MCP (remote-controlled)
    // @returns {Promise<boolean>}
    isMcpControlled: () => ipcRenderer.invoke('is-mcp-controlled')
});
