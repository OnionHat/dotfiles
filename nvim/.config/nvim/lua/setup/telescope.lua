local ok_status, telescope = pcall(require, "telescope")
if not ok_status then
	return
end

telescope.load_extension("fzf")
telescope.load_extension("file_browser")

vim.api.nvim_set_keymap(
	"n",
	"<leader>ff",
	[[<Cmd>lua require('telescope.builtin').find_files()<CR>]],
	{ noremap = true }
)
vim.api.nvim_set_keymap(
	"n",
	"<leader>ft",
	[[<Cmd>lua require 'telescope'.extensions.file_browser.file_browser()<CR>]],
	{ noremap = true }
)
vim.api.nvim_set_keymap("n", "<leader>fb", [[<Cmd>lua require('telescope.builtin').buffers()<CR>]], { noremap = true })
vim.api.nvim_set_keymap(
	"n",
	"<leader>fg",
	[[<Cmd>lua require('telescope.builtin').live_grep()<CR>]],
	{ noremap = true }
)

telescope.setup({
	defaults = {
		file_ignore_patterns = { "%.o" },
	},
})
