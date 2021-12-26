vim.cmd [[
nnoremap <F5> : silent w <bar> ! tmux splitw -v "python %; read" \; resize-pane -y 20 <CR>

setlocal noexpandtab
nnoremap <leader>fp :lua vim.lsp.buf.formatting()<CR> m`gg=G``
]]
