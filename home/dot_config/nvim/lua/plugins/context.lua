-- show current function/context up top
return {
	{
		"nvim-treesitter/nvim-treesitter-context",
		config = function ()
			require("treesitter-context").setup()
		end
	}
}
