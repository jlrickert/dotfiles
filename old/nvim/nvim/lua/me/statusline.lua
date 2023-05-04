-- Set lualine as statusline
-- See `:help lualine.txt`
local M = {}

M.setup = function()
    require('lualine').setup({
        options = {
            icons_enabled = true,
            theme = 'gruvbox',
            component_separators = '|',
            section_separators = '',
        },
        sections = {
            lualine_c = {
                {
                    'filename',
                    -- 0: Just the filename
                    -- 1: Relative path
                    -- 2: Absolute path
                    -- 3: Absolute path, with tilde as the home directory
                    path = 1,
                },
            },
        },
    })
end

return M
