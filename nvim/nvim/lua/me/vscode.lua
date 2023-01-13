local M = {}

-- Largely a work in progress
M.setup = function()
    -- Automatically source this file on file change
    local vscode_group = vim.api.nvim_create_augroup('VSCode', { clear = true })
    vim.api.nvim_create_autocmd('BufWritePost', {
        command = 'source <afile>',
        group = vscode_group,
        pattern = vim.fn.expand('<afile>'),
    })
    vim.keymap.set('n', '<leader>ca', '<cmd>call VSCodeCall("editor.action.quickFix")<cr>', { silent = true })
    vim.keymap.set('n', '<leader>f', '<cmd>call VSCodeCall("editor.action.format)<cr>', { silent = true })
    vim.api.nvim_create_autocmd('FileType', {
        callback = function(args)
            print('called')

            print(args.afile)
            print(args.amatch)
        end,
    })
end

return M
