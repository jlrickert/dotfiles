return {
	{
		"nvim-treesitter/nvim-treesitter",
		build = function()
			require("nvim-treesitter.install").update({ with_sync = true })
		end,
		event = { "BufEnter" },
		dependencies = {
			-- Additional text objects for treesitter
			"nvim-treesitter/nvim-treesitter-textobjects",
		},
		config = function()
			-- This suppresses a deprication message
			vim.g.skip_ts_context_commentstring_module = true

			---@diagnostic disable: missing-fields
			require("nvim-treesitter.configs").setup({
				ensure_installed = {
					"astro",
					"bash",
					"c",
					"css",
					"csv",
					"diff",
					"dockerfile",
					"dot",
					"git_config",
					"git_rebase",
					"gitcommit",
					"gitignore",
					"go",
					"gomod",
					"gosum",
					"gowork",
					"gpg",
					"graphql",
					"html",
					"javascript",
					"json",
					"kdl",
					"lua",
					"markdown",
					"ocaml",
					"ocaml_interface",
					"php",
					"phpdoc",
					"prisma",
					"python",
					"rust",
					"scss",
					"sql",
					"ssh_config",
					"svelte",
					"tsx",
					"typescript",
					"vim",
					"yaml", -- This is currently borked see: https://github.com/ikatyang/tree-sitter-yaml/issues/53
				},
				sync_install = false,
				highlight = {
					enable = true,
				},
				indent = {
					enable = true,
					disable = { "ocaml", "ocaml_interface" },
				},
				autopairs = {
					enable = true,
				},
				autotag = {
					enable = true,
				},
				context_commentstring = {
					enable = true,
					enable_autocmd = false,
				},
				incremental_selection = {
					enable = true,
					keymaps = {
						init_selection = "<c-space>",
						node_incremental = "<c-space>",
						scope_incremental = "<c-s>",
						node_decremental = "<c-backspace>",
					},
				},
				textobjects = {
					select = {
						enable = true,
						lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
						keymaps = {
							-- You can use the capture groups defined in textobjects.scm
							["a="] = { query = "@assignment.outer", desc = "Select outer part of an assignment" },
							["i="] = { query = "@assignment.inner", desc = "Select inner part of an assignment" },
							["l="] = { query = "@assignment.lhs", desc = "Select left hand side of an assignment" },
							["r="] = { query = "@assignment.rhs", desc = "Select right hand side of an assignment" },

							["aa"] = { query = "@parameter.outer", desc = "Select outer part of a parameter/argument" },
							["ia"] = { query = "@parameter.inner", desc = "Select inner part of a parameter/argument" },

							["ai"] = { query = "@conditional.outer", desc = "Select outer part of a conditional" },
							["ii"] = { query = "@conditional.inner", desc = "Select inner part of a conditional" },

							["al"] = { query = "@loop.outer", desc = "Select outer part of a loop" },
							["il"] = { query = "@loop.inner", desc = "Select inner part of a loop" },

							["af"] = { query = "@function.outer", desc = "Select outer part of a function" },
							["if"] = { query = "@function.inner", desc = "Select inner part of a function" },

							["am"] = { query = "@function.outer", desc = "Select outer part of a method/function definition" },
							["im"] = { query = "@function.inner", desc = "Select inner part of a method/function definition" },

							["ac"] = { query = "@class.outer", desc = "Select outer part of a class" },
							["ic"] = { query = "@class.inner", desc = "Select inner part of a class" },
						},
					},
					move = {
						enable = true,
						set_jumps = true, -- whether to set jumps in the jumplist
						goto_next_start = {
							["]m"] = "@function.outer",
							["]]"] = "@class.outer",
						},
						goto_next_end = {
							["]M"] = "@function.outer",
							["]["] = "@class.outer",
						},
						goto_previous_start = {
							["[m"] = "@function.outer",
							["[["] = "@class.outer",
						},
						goto_previous_end = {
							["[M"] = "@function.outer",
							["[]"] = "@class.outer",
						},
					},
				},
			})
		end,
	},
}
