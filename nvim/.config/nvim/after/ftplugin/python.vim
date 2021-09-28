nnoremap <F5> : silent w <bar> ! tmux splitw -v "python %; read" \; resize-pane -y 20 <CR>

setlocal noexpandtab
setlocal keywordprg=pydoc
