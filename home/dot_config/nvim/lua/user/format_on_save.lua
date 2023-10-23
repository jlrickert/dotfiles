local autoformat_enabled = true

local function toggle_autoformat()
	autoformat_enabled = not autoformat_enabled
	vim.api.nvim_out_write("Autoformat " .. (autoformat_enabled and "enabled" or "disabled") .. "\n")
end

local autoformat_group = vim.api.nvim_create_augroup("format_on_save", { clear = true })

vim.api.nvim_create_autocmd("BufWritePre", {
	group = autoformat_group,
	pattern = "*",
	desc = "Run LSP formatting on a file on save",
	callback = function()
		if not autoformat_enabled then
			return
		end
		if vim.fn.exists(":Format") > 0 then
			vim.cmd.Format()
		end
	end,
})

vim.api.nvim_create_user_command("ToggleFormatOnSave", function()
	toggle_autoformat()
end, {})
