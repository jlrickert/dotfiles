return {
	{
		"ellisonleao/gruvbox.nvim",
		priority = 1000,
		opts = {},
		config = function()
			require("gruvbox").setup({})

			vim.opt.termguicolors = true
			vim.opt.background = "dark" -- or "light" for light mode
			vim.cmd.colorscheme("gruvbox")

			-- Hide all semantic highlights until upstream issues are resolved (https://github.com/catppuccin/nvim/issues/480)
			for _, group in ipairs(vim.fn.getcompletion("@lsp", "highlight")) do
				vim.api.nvim_set_hl(0, group, {})
			end
		end,
	},
}
