local M = {}
M.setup = function()
    -- Gitsigns
    -- See `:help gitsigns.txt`
    require('gitsigns').setup {
        signs = {
            add = { text = '+' },
            change = { text = '~' },
            delete = { text = '_' },
            topdelete = { text = '‾' },
            changedelete = { text = '~' },
        },
        on_attach = function(bufnr)
            local function map(mode, lhs, rhs, opts)
                opts = vim.tbl_extend("force", { noremap = true, silent = true }, opts or {})
                vim.api.nvim_buf_set_keymap(bufnr, mode, lhs, rhs, opts)
            end

            -- Navigation
            map("n", "]c", "&diff ? ']c' : '<cmd>Gitsigns next_hunk<CR>'", { expr = true })
            map("n", "[c", "&diff ? '[c' : '<cmd>Gitsigns prev_hunk<CR>'", { expr = true })

            -- Actions
            map("n", "<leader>gs", ":Gitsigns stage_hunk<CR>")
            map("v", "<leader>gs", ":Gitsigns stage_hunk<CR>")
            map("n", "<leader>gr", ":Gitsigns reset_hunk<CR>")
            map("v", "<leader>gr", ":Gitsigns reset_hunk<CR>")
            map("n", "<leader>gS", "<cmd>Gitsigns stage_buffer<CR>")
            map("n", "<leader>gu", "<cmd>Gitsigns undo_stage_hunk<CR>")
            map("n", "<leader>gR", "<cmd>Gitsigns reset_buffer<CR>")
            map("n", "<leader>gp", "<cmd>Gitsigns preview_hunk<CR>")
            map("n", "<leader>gb", '<cmd>lua require"gitsigns".blame_line{full=true}<CR>')
            map("n", "<leader>gb", "<cmd>Gitsigns toggle_current_line_blame<CR>")
            map("n", "<leader>gd", "<cmd>Gitsigns diffthis<CR>")
            map("n", "<leader>gD", '<cmd>lua require"gitsigns".diffthis("~")<CR>')
            map("n", "<leader>td", "<cmd>Gitsigns toggle_deleted<CR>")

            -- Text object
            map("o", "ih", ":<C-U>Gitsigns select_hunk<CR>")
            map("x", "ih", ":<C-U>Gitsigns select_hunk<CR>")
        end,
    }
end
return M
