if not pcall(require, "nvim-treesitter") then
  return
end

require'nvim-treesitter.configs'.setup {
	highlight = { enable = true },
	incremental_selection = { enable = true },
	textobjects = { enable = true }
}
