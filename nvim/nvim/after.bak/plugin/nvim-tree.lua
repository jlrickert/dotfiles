-- See :help nvim-tree-setup
require('nvim-tree').setup({
    hijack_cursor = true,
    diagnostics = {
        enable = true
    },
    on_attach = function(bufnr)
        local bufmap = function(lhs, rhs, desc)
            vim.keymap.set('n', lhs, rhs, { buffer = bufnr, desc = desc })
        end

        -- :help nvim-tree.api
        local api = require('nvim-tree.api')

        bufmap('L', api.node.open.edit, 'Expand folder or go to file')
        bufmap('H', api.node.navigate.parent_close, 'Close parent folder')
        bufmap('gh', api.tree.toggle_hidden_filter, 'Toggle hidden files')
    end
})

vim.keymap.set('n', '<leader>e', '<cmd>NvimTreeToggle<cr>')
vim.keymap.set("n", "<leader>mn", require("nvim-tree.api").marks.navigate.next)
vim.keymap.set("n", "<leader>mp", require("nvim-tree.api").marks.navigate.prev)
vim.keymap.set("n", "<leader>ms", require("nvim-tree.api").marks.navigate.select)
