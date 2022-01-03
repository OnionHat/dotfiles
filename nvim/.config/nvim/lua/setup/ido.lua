if not pcall(require, "ido") then
	return
end

require("ido").setup{
   packages = {
      files = {}
   }
}

vim.api.nvim_set_keymap('n', '<leader>xf', [[<Cmd>lua require("ido").module.run("files/search")<CR>]], {noremap=true})
vim.api.nvim_set_keymap('n', '<leader>xb', [[<Cmd>lua require("ido").module.run("files/browse")<CR>]], {noremap=true})
vim.api.nvim_set_keymap('n', '<leader>xn', [[<Cmd>lua require("ido").module.run("files/navigate")<CR>]], {noremap=true})

