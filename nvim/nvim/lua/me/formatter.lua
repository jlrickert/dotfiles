local M = {}

M.setup = function()
    require('formatter').setup({
        logging = true,
        log_level = vim.log.levels.warn,
        filetype = {
            lua = {
                require('formatter.filetypes.lua').stylua,
            },
            go = {
                require('formatter.filetypes.go').gofmt,
            },
            css = {
                require('formatter.filetypes.css').prettier,
            },
            graphql = {
                require('formatter.filetypes.graphql').prettier,
            },
            html = {
                require('formatter.filetypes.html').prettier,
            },
            javascript = {
                require('formatter.filetypes.javascript').prettier,
            },
            javascriptreact = {
                require('formatter.filetypes.javascriptreact').prettier,
            },
            json = {
                require('formatter.filetypes.json').prettier,
            },
            markdown = {
                require('formatter.filetypes.markdown').prettier,
            },
            rust = {
                require('formatter.filetypes.rust').rustfmt,
            },
            sh = {
                require('formatter.filetypes.sh').shfmt,
            },
            svelte = {
                require('formatter.filetypes.svelte').prettier,
            },
            toml = {
                require('formatter.filetypes.toml').taplo,
            },
            typescript = {
                require('formatter.filetypes.typescript').prettier,
            },
            typescriptreact = {
                require('formatter.filetypes.typescriptreact').prettier,
            },
            yaml = {
                require('formatter.filetypes.yaml').prettier,
            },
        },
    })
end

return M
