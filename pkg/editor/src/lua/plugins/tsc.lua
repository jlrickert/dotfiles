-- run typechecker in the background for typescript
return {
	{
		"dmmulroy/tsc.nvim",
		ft = {
			"typescript",
			"typescriptreact",
			"svelte",
		},
		config = function()
			require("tsc").setup()
		end,
	},
}
