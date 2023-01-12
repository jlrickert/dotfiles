local mark = require('harpoon.mark')
local ui = require('harpoon.ui')

local opt = { silent = true }

vim.keymap.set('n', '<leader>ba', mark.add_file, opt)
vim.keymap.set('n', '<leader>be', ui.toggle_quick_menu, opt)
vim.keymap.set('n', '<leader>b1', function() ui.nav_file(1) end, opt)
vim.keymap.set('n', '<leader>b2', function() ui.nav_file(2) end, opt)
vim.keymap.set('n', '<leader>b3', function() ui.nav_file(3) end, opt)
vim.keymap.set('n', '<leader>b4', function() ui.nav_file(4) end, opt)
