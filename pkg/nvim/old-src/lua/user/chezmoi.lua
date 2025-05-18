vim.api.nvim_create_autocmd("BufWritePost", {
	group = vim.api.nvim_create_augroup("chezmoi", { clear = true }),
	pattern = vim.fn.expand("~") .. "/.local/share/chezmoi/home/*",
	desc = "Run chezmoi apply on file on save",
	callback = function()
		local file = vim.fn.expand("%")
		local command = "chezmoi apply --source-path " .. file
		vim.fn.system(command)
	end,
})
