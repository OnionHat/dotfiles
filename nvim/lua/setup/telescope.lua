local ok_status, telescope = pcall(require, "telescope")
if not ok_status then
	return
end

telescope.load_extension("fzf")
telescope.load_extension("file_browser")
telescope.load_extension("media_files")

telescope.setup({
	defaults = {
		file_ignore_patterns = {
            "%.o",
            "__pycache__/",
            "%.class"
        },
	},
	pickers = {
		buffers = {
			show_all_buffers = true,
			sort_lastused = true,
			theme = "dropdown",
			previewer = false,
			mappings = {
				i = {
					["<c-d>"] = "delete_buffer",
				},
			},
		},
	},
})

vim.cmd([[
nnoremap <silent> <leader>ff :Telescope find_files<CR>
nnoremap <silent> <leader>fb :Telescope buffers<CR>
nnoremap <silent> <leader>fg :Telescope live_grep<CR>
nnoremap <silent> <leader>fq :Telescope quickfix<CR>
nnoremap <silent> <leader>fl :Telescope loclist<CR>
nnoremap <silent> <leader>fk :Telescope keymaps<CR>
nnoremap <silent> <leader>fS :Telescope current_buffer_fuzzy_find<CR>
nnoremap <silent> <leader>fD :lua require("telescope.builtin").diagnostics({severity=1})<CR>
nnoremap <silent> <leader>fd :lua require("telescope.builtin").diagnostics({bufnr=0,severity=1})<CR>
nnoremap <silent> <leader>fs :Telescope lsp_document_symbols<CR>


nnoremap <silent> <leader>fe :Telescope file_browser<CR>
nnoremap <silent> <leader>fm :Telescope media_files<CR>
]])
