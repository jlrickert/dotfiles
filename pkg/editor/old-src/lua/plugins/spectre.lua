-- global search and replace
return {
	{
		"nvim-pack/nvim-spectre",
		lazy = true,
		dependencies = {
			"nvim-lua/plenary.nvim",
			"ellisonleao/gruvbox.nvim",
		},
		config = function()
			local theme = require("gruvbox").palette

			vim.api.nvim_set_hl(0, "SpectreSearch", {
				bg = theme.dark_red,
				fg = theme.light1,
			})
			vim.api.nvim_set_hl(0, "SpectreReplace", {
				bg = theme.dark_green,
				fg = theme.light1,
			})

			require("spectre").setup({
				highlight = {
					search = "SpectreSearch",
					replace = "SpectreReplace",
				},
				mapping = {
					["send_to_qf"] = {
						map = "<C-q>",
						cmd = "<cmd>lua require('spectre.actions').send_to_qf()<CR>",
						desc = "send all items to quickfix",
					},
				},
			})
		end,
	},
}
