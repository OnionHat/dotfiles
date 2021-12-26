if not pcall(require, "telescope") then
	return
end

require("telescope").setup{
	defaults = {
		file_ignore_patterns = {"%.o"},
	},
}

-- if not pcall(require, "fzf") then
-- 	return
-- end
require('telescope').load_extension('fzf')


