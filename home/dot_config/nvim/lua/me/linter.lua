local M = {}

M.setup = function()
    -- vim.api.nvim_create_autocmd({ 'BufWritePost' }, {
    --     group = vim.api.nvim_create_augroup('Linting', { clear = true }),
    --     callback = function()
    --         require('lint').try_lint()
    --     end,
    -- })
end

return M
