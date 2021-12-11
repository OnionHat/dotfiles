if not pcall(require, "telescope") then
	return
end

require("telescope").setup{
	defaults = {
		file_ignore_patterns = {"%.o"}
	}
}


