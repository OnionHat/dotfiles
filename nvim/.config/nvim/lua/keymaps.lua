-- TELESCOPE
vim.api.nvim_set_keymap('n', '<leader>ff', [[<Cmd>lua require('telescope.builtin').find_files()<CR>]], {noremap=true})
vim.api.nvim_set_keymap('n', '<leader>ft', [[<Cmd>lua require('telescope.builtin').file_browser()<CR>]], {noremap=true})
vim.api.nvim_set_keymap('n', '<leader>fb', [[<Cmd>lua require('telescope.builtin').buffers()<CR>]], {noremap=true})

-- NERDTREE
--vim.api.nvim_set_keymap('n', '<leader>n', ':NERDTreeFocus<CR>', {noremap=true})
--vim.api.nvim_set_keymap('n', '<C-n>', ':NERDTree<CR>', {noremap=true})
--vim.api.nvim_set_keymap('n', '<C-t>', ':NERDTreeToggle<CR>', {noremap=true})
--vim.api.nvim_set_keymap('n', '<C-f>', ':NERDTreeFind<CR>', {noremap=true})

-- DISABLING Q
vim.api.nvim_set_keymap('n', 'Q', '', {noremap=true})

--vim.api.nvim_set_keymap()
