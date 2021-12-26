if not pcall(require, "lualine") then
  return
end

local function filepath()
	local path = vim.fn.expand('%:p')
	if vim.fn.winwidth(0) <= 84 then
		path = vim.fn.pathshorten(path)
	end
	return path
end

require('lualine').setup{
	options = {theme = 'jellybeans'},
	sections = {lualine_c = { filepath }}
}
