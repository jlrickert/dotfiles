local M = {}


M.keg_directory = function()
	local dir = vim.fn.system("keg directory")
	if dir == "no kegs found" then
		return nil
	end
	return dir
end

M.keg_next_node_id = function()
	local last_id = vim.fn.system("keg last id")
	if last_id == "no kegs found" then
		return nil
	end
	return tonumber(last_id) + 1
end

M.keg_index_update = function()
	vim.fn.system("keg index update")
end

M.Keg_publish = function()
	vim.fn.system("keg publish")
end

local function get_or_create_buffer(filename)
	local buf_exists = vim.fn.bufexists(filename) ~= 0
	if buf_exists then
		return vim.fn.bufnr(filename)
	end

	return vim.fn.bufadd(filename)
end

--
---comment
---@param opt table<string, any>
local function create_keg_template(opt)
	vim.api.nvim_create_user_command(opt['command'], function()
		local dir = M.keg_directory()
		local id = M.keg_next_node_id()
		if not id or not dir then
			return
		end
		local node_path = dir .. "/" .. id
		local node_fname = vim.fs.normalize(node_path .. "/README.md")
		local title = opt['title']()
		local contents = opt['content']()
		vim.fn.mkdir(node_path, "p")                -- Create parent directories if they don't exist
		vim.fn.writefile({ title, contents }, node_fname) -- Create an empty file

		local meta_fname = vim.fs.normalize(node_path .. "/meta.yaml")
		vim.fn.writefile(opt['meta'](), meta_fname) -- Create an empty file

		-- Create and open node in new buffer
		local buf_id = get_or_create_buffer(node_fname)
		vim.api.nvim_set_current_buf(buf_id)
		-- vim.api.nvim_buf_set_option(buf_id, "buflisted", true)
		vim.bo[buf_id].buflisted = true

		-- remove the previous
		local old_bufnr = vim.api.nvim_get_current_buf()
		local old_bufinfo = vim.fn.getbufinfo(old_bufnr)
		if type(old_bufinfo) == "table" and #old_bufinfo >= 1 then
			old_bufinfo = old_bufinfo[1]
			local no_name = old_bufinfo.name == ""
			local one_line = old_bufinfo.linecount == 1
			local unchanged = old_bufinfo.changed == 0
			if no_name and one_line and unchanged then
				vim.api.nvim_buf_delete(old_bufnr, {})
			end
		end
	end, { desc = opt['desc'] })
end

M.setup = function()
	-- vim.api.nvim_create_autocmd('FileType', {
	-- 	pattern = "keg"
	-- })
	-- vim.api.nvim_create_autocmd("FileType", {
	-- 	pattern = "markdown",
	-- 	callback = function()
	-- 		local bufnr = vim.api.nvim_get_current_buf()
	--
	-- 		-- Check if the LSP client is already attached to the buffer
	-- 		if vim.b[bufnr].markdown_lsp_attached then
	-- 			return
	-- 		end
	--
	-- 		-- Start the LSP client
	-- 		local client_id = vim.lsp.start({
	-- 			name = "markdown-awesome-server",
	-- 			cmd = { "/Users/jlrickert/repos/github.com/jlrickert/live-demo-proj/demo_proj" },
	-- 		})
	--
	-- 		-- Mark the buffer as having the LSP client attached
	-- 		if client_id then
	-- 			vim.b[bufnr].markdown_lsp_attached = true
	-- 		end
	-- 	end,
	-- })

	vim.api.nvim_create_user_command("KegCreate", function()
		local dir = M.keg_directory()
		local id = M.keg_next_node_id()
		if not id or not dir then
			return
		end
		local node_path = dir .. "/" .. id
		local node_fname = vim.fs.normalize(node_path .. "/README.md")
		vim.fn.mkdir(node_path, "p") -- Create parent directories if they don't exist
		vim.fn.writefile({}, node_fname) -- Create an empty file

		local meta_fname = vim.fs.normalize(node_path .. "/meta.yaml")
		vim.fn.writefile({ "tags:" }, meta_fname) -- Create an empty file

		-- Create and open node in new buffer
		local buf_id = get_or_create_buffer(node_fname)
		vim.api.nvim_set_current_buf(buf_id)
		vim.api.nvim_buf_set_option(buf_id, "buflisted", true)

		-- remove the previous
		local old_bufnr = vim.api.nvim_get_current_buf()
		local old_bufinfo = vim.fn.getbufinfo(old_bufnr)
		if type(old_bufinfo) == "table" and #old_bufinfo >= 1 then
			old_bufinfo = old_bufinfo[1]
			local no_name = old_bufinfo.name == ""
			local one_line = old_bufinfo.linecount == 1
			local unchanged = old_bufinfo.changed == 0
			if no_name and one_line and unchanged then
				vim.api.nvim_buf_delete(old_bufnr, {})
			end
		end
	end, { desc = "Create keg node and open it up its README.md" })

	create_keg_template({
		command = "KegCreateBake",
		title = function()
			return "# Title for bake"
		end,
		contents = function()
			return {
				"",
				"## Ingredients",
				"",
				"| Ingredient  | Amount | Bakers Percentage | Comment                  |",
				"| ----------- | ------ | ----------------- | ------------------------ |",
				"",
				"## Time log",
				""
			}
		end,
		meta = function()
			local current_date = os.date("%Y-%m-%d")
			return { "tags:", "  - baking", "  - sourdough", "date: " .. current_date }
		end,
		desc = "Create keg node with Baking template"
	})

	-- FIXME: This is the wrong way to do this. Luasnip should be handling
	-- this. Need to figure out how to write snippets using lua for this
	-- though. This was easier to do.
	vim.api.nvim_create_user_command("KegCreateDaily", function()
		local dir = M.keg_directory()
		local id = M.keg_next_node_id()
		if not id or not dir then
			return
		end
		local node_path = dir .. "/" .. id
		local node_fname = vim.fs.normalize(node_path .. "/README.md")
		local title = "# Daily: " .. os.date("%A %B %d %Y")
		local current_date = os.date("%Y-%m-%d")
		local description = "Daily entry for " .. current_date
		vim.fn.mkdir(node_path, "p")      -- Create parent directories if they don't exist
		vim.fn.writefile({ title, "", description }, node_fname) -- Create an empty file

		local meta_fname = vim.fs.normalize(node_path .. "/meta.yaml")
		vim.fn.writefile({ "tags:", "  - daily", "date: " .. current_date }, meta_fname) -- Create an empty file

		-- Create and open node in new buffer
		local buf_id = get_or_create_buffer(node_fname)
		vim.api.nvim_set_current_buf(buf_id)
		vim.api.nvim_buf_set_option(buf_id, "buflisted", true)

		-- remove the previous
		local old_bufnr = vim.api.nvim_get_current_buf()
		local old_bufinfo = vim.fn.getbufinfo(old_bufnr)
		if type(old_bufinfo) == "table" and #old_bufinfo >= 1 then
			old_bufinfo = old_bufinfo[1]
			local no_name = old_bufinfo.name == ""
			local one_line = old_bufinfo.linecount == 1
			local unchanged = old_bufinfo.changed == 0
			if no_name and one_line and unchanged then
				vim.api.nvim_buf_delete(old_bufnr, {})
			end
		end
	end, { desc = "Create keg node, add some boilerplate content, and open up its README.md" })

	vim.api.nvim_create_user_command("KegCreateTask", function()
		local dir = M.keg_directory()
		local id = M.keg_next_node_id()
		if not id or not dir then
			return
		end
		local node_path = dir .. "/" .. id
		local node_fname = vim.fs.normalize(node_path .. "/README.md")
		local title = "# [ ] Project Task | description"
		vim.fn.mkdir(node_path, "p")      -- Create parent directories if they don't exist
		vim.fn.writefile({ title }, node_fname) -- Create an empty file

		local meta_fname = vim.fs.normalize(node_path .. "/meta.yaml")
		vim.fn.writefile({ "tags:", "  - task", }, meta_fname)

		-- Create and open node in new buffer
		local buf_id = get_or_create_buffer(node_fname)
		vim.api.nvim_set_current_buf(buf_id)
		vim.api.nvim_buf_set_option(buf_id, "buflisted", true)

		-- remove the previous buffer
		local old_bufnr = vim.api.nvim_get_current_buf()
		local old_bufinfo = vim.fn.getbufinfo(old_bufnr)
		if type(old_bufinfo) == "table" and #old_bufinfo >= 1 then
			old_bufinfo = old_bufinfo[1]
			local no_name = old_bufinfo.name == ""
			local one_line = old_bufinfo.linecount == 1
			local unchanged = old_bufinfo.changed == 0
			if no_name and one_line and unchanged then
				vim.api.nvim_buf_delete(old_bufnr, {})
			end
		end
	end, { desc = "Create keg node with a basic template for a task, and open up its README.md" })

	vim.api.nvim_create_user_command("KegCreateMeeting", function()
		local dir = M.keg_directory()
		local id = M.keg_next_node_id()
		if not id or not dir then
			return
		end
		local node_path = dir .. "/" .. id
		local node_fname = vim.fs.normalize(node_path .. "/README.md")
		local title = "# meeting on " .. os.date("%A %B %d %Y")
		vim.fn.mkdir(node_path, "p")      -- Create parent directories if they don't exist
		vim.fn.writefile({ title }, node_fname) -- Create an empty file

		local meta_fname = vim.fs.normalize(node_path .. "/meta.yaml")
		local current_date = os.date("%Y-%m-%d")
		vim.fn.writefile({ "tags:", "  - meeting", "date: " .. current_date }, meta_fname) -- Create an empty file

		-- Create and open node in new buffer
		local buf_id = get_or_create_buffer(node_fname)
		vim.api.nvim_set_current_buf(buf_id)
		vim.api.nvim_buf_set_option(buf_id, "buflisted", true)

		-- remove the previous
		local old_bufnr = vim.api.nvim_get_current_buf()
		local old_bufinfo = vim.fn.getbufinfo(old_bufnr)
		if type(old_bufinfo) == "table" and #old_bufinfo >= 1 then
			old_bufinfo = old_bufinfo[1]
			local no_name = old_bufinfo.name == ""
			local one_line = old_bufinfo.linecount == 1
			local unchanged = old_bufinfo.changed == 0
			if no_name and one_line and unchanged then
				vim.api.nvim_buf_delete(old_bufnr, {})
			end
		end
	end, { desc = "Create keg node and open it up its README.md" })

	vim.api.nvim_create_user_command("KegIndex", function()
		M.keg_index_update()
	end, { desc = "Re-index a keg" })

	vim.api.nvim_create_user_command("KegPublish", function()
		M.Keg_publish()
	end, { desc = "Publish a keg" })

	local keg_group = vim.api.nvim_create_augroup("keg", { clear = true })

	vim.api.nvim_create_autocmd("BufWritePost", {
		group = keg_group,
		pattern = "*/README.md",
		desc = "Update keg index",
		callback = function()
			if M.keg_directory() then
				M.keg_index_update()
			end
		end,
	})

	vim.api.nvim_create_autocmd({ 'BufRead', 'BufNewFile' }, {
		-- group = vim.api.nvim_create_augroup('keg', { clear = false }),
		pattern = "keg",
		desc = "Set filetype for keg file to yaml",
		callback = function()
			vim.bo.filetype = "yaml"
		end
	})
end

return M
