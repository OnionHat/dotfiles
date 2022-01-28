vim.opt.keywordprg = ":help"
vim.cmd([[
nnoremap <buffer> <leader>fo :lua vim.lsp.buf.formatting()<CR>
]])
