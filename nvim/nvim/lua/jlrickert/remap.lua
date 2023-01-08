vim.g.mapleader = " "
vim.g.maplocalleader = " "
vim.keymap.set("n", "<leader>pv", vim.cmd.Ex)

vim.keymap.set("n", "<leader>y", "\"+y")
vim.keymap.set("n", "<leader>Y", "\"+Y")
vim.keymap.set('n', '<leader>j=', '<cmd>:LspZeroFormat<CR>')
--vim.keymap.set('n', '<leader>j=', '<cmd>:Lsp<CR>')
vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action)

vim.keymap.set("i", "<>", "<><Left>")
vim.keymap.set("i", "()", "()<Left>")
vim.keymap.set("i", "{}", "{}<Left>")
vim.keymap.set("i", "[]", "[]<Left>")
vim.keymap.set("i", "\"\"", "\"\"<Left>")
vim.keymap.set("i", "''", "''<Left>")
vim.keymap.set("i", "``", "``<Left>")
