nnoremap <F5> : silent w <bar> ! tmux splitw -v "python %; read" \; resize-pane -y 20 <CR>

"nnoremap <F5> :w <bar> ! tmux splitw -v "if [[ -n $VIRTUAL_ENV ]]; then source $VIRTUAL_ENV/bin/activate;fi; python %; read" \; resize-pane -y 20 <CR> <CR>
