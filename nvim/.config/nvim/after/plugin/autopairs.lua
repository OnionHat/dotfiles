if not pcall(require, "nvim-autopairs") then
	return
else
	return
end
require('nvim-autopairs').setup{
  disable_filetype = { "TelescopePrompt" , "vim" },
  map_cr = true
}
