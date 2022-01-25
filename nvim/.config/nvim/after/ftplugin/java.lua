vim.cmd[[
compiler javac

set makeprg=javac\ %

autocmd QuickfixCmdPost make call AfterMakeJava()
function! AfterMakeJava()
    " No any error after make
    if len(getqflist()) == 0
        !java -cp bin %:r
    endif
    " :~)
endfunction

nnoremap <F5> :wa <bar> make<cr>
]]
