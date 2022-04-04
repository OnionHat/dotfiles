local ok_status, lualine = pcall(require, "lualine")
if not ok_status then
	return
end

local gps = require("nvim-gps")

lualine.setup({
	options = { theme = "jellybeans" },
	sections = {
		lualine_a = {
			{
				"filename",
				file_status = true, -- Displays file status (readonly status, modified status)
				path = 0, -- 0: Just the filename
				-- 1: Relative path
				-- 2: Absolute path

				shorting_target = 40, -- Shortens path to leave 40 spaces in the window
				-- for other components. (terrible name, any suggestions?)
				symbols = {
					modified = "[+]", -- Text to show when the file is modified.
					readonly = "[-]", -- Text to show when the file is non-modifiable or readonly.
					unnamed = "[No Name]", -- Text to show for unnamed buffers.
				},
			},
		},
		lualine_c = {
			{ gps.get_location, cond = gps.is_available },
		},
	},
})

-- if
-- 	(packer_plugins["nvim-gps"] or packer_plugins["nvim-gps"].loaded)
-- 	and (packer_plugins["lsp_signature.nvim"] or packer_plugins["lsp_signature.nvim"].loaded)
-- then
-- 	lualine.setup({ section = { lualine_c = { gps.get_location, cond = gps.is_available, func_sig } } })
-- elseif packer_plugins["nvim-gps"] or packer_plugins["nvim-gps"].loaded then
-- 	lualine.setup({ section = { lualine_c = { { gps.get_location, cond = gps.is_available } } } })
-- elseif packer_plugins["lsp_signature.nvim"] or packer_plugins["lsp_signature.nvim"].loaded then
-- 	lualine.setup({ section = { lualine_c = { { func_sig } } } })
-- end
