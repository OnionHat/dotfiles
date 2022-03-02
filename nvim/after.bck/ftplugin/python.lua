vim.cmd [[
" nnoremap <F5> : silent w <bar> ! tmux splitw -v "python %; read" \; resize-pane -y 20 <CR>

" setlocal noexpandtab
if !exists("current_compiler")
  compiler python
endif

" setlocal makeprg=python\ %
" nnoremap <buffer> <F6> :setlocal makeprg=python\ % <bar> Make<CR>
" nnoremap <buffer> <silent> <F5> :Make<CR>
]]
