-- Backup files
-- Idea from https://www.reddit.com/r/neovim/comments/wlkq0e/neovim_configuration_to_backup_files_with/
-- Double slash to build file name from the complete path to the file with all path separators changed to percent '%' signs
-- vim.opt.backupdir = '/tmp/nvim-backup//'
vim.opt.swapfile = true
-- vim.opt.backup = true
vim.opt.undofile = true


-- Add timestamp as extension for backup files
-- vim.api.nvim_create_autocmd('BufWritePre', {
-- 	group = vim.api.nvim_create_augroup('timestamp_backupext', { clear = true }),
-- 	desc = 'Add timestamp to backup extension',
-- 	pattern = '*',
-- 	callback = function()
-- 		vim.opt.backupext = '-' .. vim.fn.strftime('%Y%m%d%H%M')
-- 	end,
-- })
