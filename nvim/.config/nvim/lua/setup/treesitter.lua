if not pcall(require, "nvim-treesitter") then
	return
end

require("nvim-treesitter.configs").setup({
	highlight = { enable = true },
	incremental_selection = { enable = true },
	textobjects = { enable = true },
	rainbow = {
		enable = true,
		-- disable = { "jsx", "cpp" }
		extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
		max_file_lines = nil, -- Do not enable for files with more than n lines, int
	},
})
