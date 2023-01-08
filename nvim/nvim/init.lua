require("jlrickert")

-- Commented out as this is compiled to `packer_compiled.lua`
--require("jlrickert.packer")

-- Automatically source and re-compile packer whenever you save this init.lua
local packer_group = vim.api.nvim_create_augroup('Packer', { clear = true })
vim.api.nvim_create_autocmd('BufWritePost', {
    command = 'source <afile> | PackerCompile',
    group = packer_group,
    pattern = '*/packer.lua',
})

