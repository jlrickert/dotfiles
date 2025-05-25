return {
	{
		"NvChad/nvim-colorizer.lua",
		config = function()
			require("colorizer").setup({
				filetypes = {
					"css",
					"javascript",
					"typescript",
					"typescriptreact",
					"python",
					"svelte",
					"lua",
					html = { mode = "foreground" },
				},
			})
		end,
	},
}
