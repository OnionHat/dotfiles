if not pcall(require, "lualine") then
	return
end

local function filepath()
	local path = vim.fn.expand("%:p")
	if vim.fn.winwidth(0) <= 84 then
		path = vim.fn.pathshorten(path)
	end
	return path
end

local current_signature = function()
	if not packer_plugins["lsp_signature.nvim"] or packer_plugins["lsp_signature.nvim"].loaded == false then
		return ""
	end
	local sig = require("lsp_signature").status_line(300)
	return sig.label .. " " .. sig.hint
end

require("lualine").setup({
	options = { theme = "jellybeans" },
	sections = { lualine_c = { filepath } },
})
