-- Enable relative line numbers
vim.opt.nu = true
vim.opt.rnu = false

-- Set tabs to 4 spaces
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.expandtab = false

-- Enable auto indenting and set it to spaces
vim.opt.smartindent = true
vim.opt.shiftwidth = 4

-- Enable smart indenting (see https://stackoverflow.com/questions/1204149/smart-wrap-in-vim)
vim.opt.breakindent = true

-- Enable incremental searching
vim.opt.incsearch = true
vim.opt.hlsearch = true

-- Disable text wrap
vim.opt.wrap = false

-- Set leader key to space
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Better splitting
vim.opt.splitbelow = true
vim.opt.splitright = true

-- Enable mouse mode
vim.opt.mouse = "a"

-- Enable ignorecase + smartcase for better searching
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Decrease updatetime to 200ms
vim.opt.updatetime = 50

-- Set completeopt to have a better completion experience
vim.opt.completeopt = { "menuone", "noselect" }

-- Enable persistent undo history
vim.opt.undofile = true

-- Enable 24-bit color
vim.opt.termguicolors = true

-- Enable the sign column to prevent the screen from jumping
vim.opt.signcolumn = "yes"

-- Use system clipboard by default
vim.opt.clipboard = "unnamed,unnamedplus"

-- See https://neovim.io/doc/user/provider.html#clipboard-osc52 for docs
-- Use OSC 52 if available. This is added to the core in neovim 0.10.0
if vim.fn.has('nvim-0.10.0') == 0 then
	-- vim.g.clipboard = {
	-- 	name = 'OSC 52',
	-- 	copy = {
	-- 		['+'] = require('vim.ui.clipboard.osc52').copy('+'),
	-- 		['*'] = require('vim.ui.clipboard.osc52').copy('*'),
	-- 	},
	-- 	paste = {
	-- 		['+'] = require('vim.ui.clipboard.osc52').paste('+'),
	-- 		['*'] = require('vim.ui.clipboard.osc52').paste('*'),
	-- 	},
	-- }
end

-- Enable cursor line highlight
vim.opt.cursorline = true

-- Set fold settings
-- These options were reccommended by nvim-ufo
-- See: https://github.com/kevinhwang91/nvim-ufo#minimal-configuration
vim.opt.foldcolumn = "0"
vim.opt.foldlevel = 99
vim.opt.foldlevelstart = 99
vim.opt.foldenable = true

-- Always keep 8 lines above/below cursor unless at start/end of file
vim.opt.scrolloff = 8

-- Place a column line
vim.opt.colorcolumn = "120"
