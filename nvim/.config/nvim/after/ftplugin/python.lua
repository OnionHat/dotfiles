vim.cmd [[
nnoremap <F5> : silent w <bar> ! tmux splitw -v "python %; read" \; resize-pane -y 20 <CR>

setlocal noexpandtab
nnoremap <buffer> <leader>fo m`gg=G`` :lua vim.lsp.buf.formatting()<CR>
" nnoremap <buffer> <leader>fo :echo "hello"
]]
