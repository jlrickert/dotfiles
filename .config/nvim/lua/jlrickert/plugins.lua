local status, packer = pcall(require, "packer")

if not status then
	print("Packer is not installed")
	return
end

vim.cmd([[packadd packer.nvim]])

return require("packer").startup(function()
	use("wbthomason/packer.nvim")

	use("sbdchd/neoformat")

	use("nvim-lualine/lualine.nvim") -- Statusline
	use("onsails/lspkind-nvim") -- vscode-like pictograms
	use("hrsh7th/cmp-buffer") -- nvim-cmp source for buffer words
	use("hrsh7th/cmp-nvim-lsp") -- nvim-cmp source for neovim's built-in LSP
	use("hrsh7th/nvim-cmp") -- Completion
	use("neovim/nvim-lspconfig") -- LSP
	use("jose-elias-alvarez/null-ls.nvim") -- Use Neovim as a language server to inject LSP diagnostics, code actions, an
	use("MunifTanjim/prettier.nvim") -- Prettier plugin for Neovim's built-in LSP client

	use("williamboman/mason.nvim") -- package manager for lsp, dap, linters, and formaters
	use("williamboman/mason-lspconfig.nvim")

	-- TJ created lodash of neovim
	use("nvim-lua/plenary.nvim") -- common utilities
	use("nvim-lua/popup.nvim")

	use("glepnir/lspsaga.nvim") -- LSP UIs
	use("L3MON4D3/LuaSnip")

	-- Interactive menu for lists
	use("nvim-telescope/telescope.nvim")
	use("nvim-telescope/telescope-file-browser.nvim")

	-- syntax highlighting
	use({
		"nvim-treesitter/nvim-treesitter",
		run = function()
			require("nvim-treesitter.install").update({ with_sync = true })
		end,
	})
	use("nvim-treesitter/nvim-treesitter-textobjects") -- Additional textobjects for treesitter.

	--use("nvim-treesitter/playground")
	use("romgrk/nvim-treesitter-context")

	use({ "srcery-colors/srcery-vim", as = "srcery" })

	if packer_bootstrap then
		require("packer").sync()
	end
end)
