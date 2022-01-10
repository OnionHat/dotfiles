local ok_status, telescope = pcall(require, "telescope")
if not ok_status then
	return
end

telescope.load_extension("fzf")
telescope.load_extension("file_browser")

telescope.setup({
	defaults = {
		file_ignore_patterns = { "%.o" },
	},
})

vim.cmd([[
nnoremap <leader>ff :lua require('telescope.builtin').find_files()<CR>
nnoremap <leader>fb :lua require('telescope.builtin').buffers()<CR>
nnoremap <leader>fg :lua require('telescope.builtin').live_grep()<CR>
nnoremap <leader>fe :lua require('telescope').extensions.file_browser.file_browser()<CR>
]])
