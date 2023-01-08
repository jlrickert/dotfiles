local ensure_packer = function()
    local fn = vim.fn
    local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
    if fn.empty(fn.glob(install_path)) > 0 then
        fn.system({ "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path })

        vim.cmd([[packadd packer.nvim]])
        return true
    end
    return false
end

ensure_packer()

return require('packer').startup(function(use)
    -- this needs to be first
    use 'wbthomason/packer.nvim'

    use {
        'nvim-telescope/telescope.nvim', tag = '0.1.0',
        -- or                            , branch = '0.1.x',
        requires = { { 'nvim-lua/plenary.nvim' } }
    }

    use {
        'nvim-treesitter/nvim-treesitter',
        config = function() pcall(require, 'plugins.treesitter') end,
    }
    use 'nvim-treesitter/nvim-treesitter-textobjects'
    use 'nvim-treesitter/nvim-treesitter-context'
    use 'nvim-treesitter/playground'
    use { "kevinhwang91/nvim-bqf", ft = 'qf' }
    use 'theprimeagen/harpoon'
    use 'mbbill/undotree'
    use {
        'numToStr/Comment.nvim',
        config = function()
            require("Comment").setup()
        end
    }
    use { 'tpope/vim-surround' }
    use { 'sindrets/diffview.nvim', requires = 'nvim-lua/plenary.nvim' }

    -- project stuff
    use 'neovim/nvim-lspconfig'
    use 'jose-elias-alvarez/null-ls.nvim'
    -- use 'MunifTanjim/prettier.nvim'
    use 'mhartington/formatter.nvim'
    -- use 'gpanders/editorconfig.nvim'
    use 'editorconfig/editorconfig-vim'

    -- Git plugins
    use 'tpope/vim-fugitive'
    use 'lewis6991/gitsigns.nvim'

    -- File explorer
    use {
        'nvim-tree/nvim-tree.lua',
        requires = {
            'nvim-tree/nvim-web-devicons', -- optional, for file icons
        },
    }

    use 'williamboman/mason.nvim'
    use {
        'VonHeikemen/lsp-zero.nvim',
        requires = {
            -- LSP Support

            { 'neovim/nvim-lspconfig' },
            { 'williamboman/mason.nvim' },
            { 'williamboman/mason-lspconfig.nvim' },

            -- Autocompletion
            { 'hrsh7th/nvim-cmp' },
            { 'hrsh7th/cmp-buffer' },
            { 'hrsh7th/cmp-path' },
            { 'saadparwaiz1/cmp_luasnip' },
            { 'hrsh7th/cmp-nvim-lsp' },
            { 'hrsh7th/cmp-nvim-lua' },

            -- Snippets
            { 'L3MON4D3/LuaSnip' },
            { 'rafamadriz/friendly-snippets' },
        }
    }

    use({ 'srcery-colors/srcery-vim', as = 'srcery' })
    use 'Shatur/neovim-ayu'
    use 'folke/tokyonight.nvim'
    use 'joshdick/onedark.vim'
    use 'tanvirtin/monokai.nvim'
    use 'lunarvim/darkplus.nvim'
    use 'kyazdani42/nvim-web-devicons'
end)
