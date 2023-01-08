require("jlrickert.remap")
require("jlrickert.commands")

--local augroup = vim.api.nvim_create_augroup
--local autocmd = vim.api.nvim_create_autocmd
--local yank_group = augroup('HighlightYank', {})

-- flash yanked content briefly
-- autocmd('TextYankPost', {
--     group = yank_group,
--     pattern = "*",
--     callback = function()
--         vim.highlight.on_yank({
--             higroup = 'IncSearch',
--             timeout = 40
--         })
--     end
-- })

--local markdownSpell = augroup('markdownSpell', {})
--autocmd({'BufRead', 'BufNewFile'}, {
--    pattern = '*.md',
--    group = markdownSpell,
--    callback = function ()
--        vim.opt_local.spell = true
--    end
--})


-- disable netrw
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
vim.g.netrw_browser_split = 0
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25

vim.opt.guicursor = ""
vim.opt.nu = true

vim.opt.smartindent = true

vim.opt.wrap = false

vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true

vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv("HOME") .. "/.vim/undodir"
vim.opt.undofile = true

vim.opt.hlsearch = false
vim.opt.incsearch = true

vim.opt.termguicolors = true
vim.opt.scrolloff = 8
vim.opt.signcolumn = 'yes:2'
vim.opt.isfname:append("@-@")

vim.opt.updatetime = 50

vim.opt.colorcolumn = "80"

vim.opt.autoread = true

-- Seems to show hidden characters when this is set
vim.opt.list = true
vim.opt.showbreak = "↪ "
vim.opt.listchars = { trail = '␣', tab = '» ' }
