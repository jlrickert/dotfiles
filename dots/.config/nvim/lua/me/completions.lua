local M = {}

M.setup = function()
    -- nvim-cmp setup
    local cmp = require('cmp')
    local luasnip = require('luasnip')

    local select_opts = { behavior = 'select' }

    cmp.setup({
        snippet = {
            expand = function(args)
                luasnip.lsp_expand(args.body)
            end,
        },
        mapping = cmp.mapping.preset.insert({
            ['<C-d>'] = cmp.mapping.scroll_docs(-5),
            ['<C-f>'] = cmp.mapping.scroll_docs(5),
            ['<C-Space>'] = cmp.mapping.complete({}),
            ['<CR>'] = cmp.mapping.confirm({
                -- behavior = cmp.ConfirmBehavior.Replace,
                select = true,
            }),

            -- navigate items on the list
            ['<Up>'] = cmp.mapping.select_prev_item(select_opts),
            ['<Down>'] = cmp.mapping.select_next_item(select_opts),
            ['<C-p>'] = cmp.mapping.select_prev_item(select_opts),
            ['<C-n>'] = cmp.mapping.select_next_item(select_opts),

            -- toggle completion
            ['<C-e>'] = cmp.mapping(function()
                if cmp.visible() then
                    cmp.abort()
                else
                    cmp.complete()
                end
            end),
        }),
        sources = {
            { name = 'nvim_lsp' },
            { name = 'luasnip' },
        },
    })
end

return M
