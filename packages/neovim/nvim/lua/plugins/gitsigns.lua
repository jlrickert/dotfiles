return {
	{
		"lewis6991/gitsigns.nvim",
		tag = "v0.8.1",
		event = "VeryLazy",
		config = function()
			require("gitsigns").setup()
		end,
	},
}
