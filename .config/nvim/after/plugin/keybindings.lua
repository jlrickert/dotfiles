local Remap = require("jlrickert.keymap")
local nnoremap = Remap.nnoremap

local builtin = require("telescope.builtin")

-- Remap space as leader key.
vim.api.nvim_set_keymap("", "<Space>", "<Nop>", { noremap = true, silent = true })
vim.g.mapleader = " "
vim.g.maplocalleader = ""

-- Remap for dealing with word wrap.
--vim.api.nvim_set_keymap('n', 'k', "v:count == 0 ? 'gk' : 'k'", { noremap = true, expr = true, silent = true })
--vim.api.nvim_set_keymap('n', 'j', "v:count == 0 ? 'gj' : 'j'", { noremap = true, expr = true, silent = true })

nnoremap("<C-p>", ":Telescope")
nnoremap("<leader> ", builtin.commands)
nnoremap("<leader>pf", function()
	builtin.find_files({ no_ignore = false, hidden = true })
end)
nnoremap("<leader>pg", builtin.live_grep)
nnoremap("<leader>pb", builtin.buffers)
nnoremap("<leader>ph", builtin.help_tags)
nnoremap("<leader>pe", builtin.diagnostics)
nnoremap("<leader>pr", builtin.live_grep)
vim.keymap.set("n", ";;", function()
	builtin.resume()
end)
vim.keymap.set("n", ";e", function()
	builtin.diagnostics()
end)
nnoremap("<leader>ps", function()
	telescope.extensions.file_browser.file_browser({
		path = "%:p:h",
		cwd = telescope_buffer_dir(),
		respect_gitignore = false,
		hidden = true,
		grouped = true,
		previewer = false,
		initial_mode = "normal",
		layout_config = { height = 40 },
	})
end)
