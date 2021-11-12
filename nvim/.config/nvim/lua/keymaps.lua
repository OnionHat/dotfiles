-- TELESCOPE
vim.api.nvim_set_keymap('n', '<leader>ff', [[<Cmd>lua require('telescope.builtin').find_files()<CR>]], {noremap=true})
vim.api.nvim_set_keymap('n', '<leader>ft', [[<Cmd>lua require('telescope.builtin').file_browser()<CR>]], {noremap=true})
vim.api.nvim_set_keymap('n', '<leader>fb', [[<Cmd>lua require('telescope.builtin').buffers()<CR>]], {noremap=true})
vim.api.nvim_set_keymap('n', '<leader>fg', [[<Cmd>lua require('telescope.builtin').live_grep()<CR>]], {noremap=true})

-- NERDTREE
--vim.api.nvim_set_keymap('n', '<leader>n', ':NERDTreeFocus<CR>', {noremap=true})
--vim.api.nvim_set_keymap('n', '<C-n>', ':NERDTree<CR>', {noremap=true})
--vim.api.nvim_set_keymap('n', '<C-t>', ':NERDTreeToggle<CR>', {noremap=true})
--vim.api.nvim_set_keymap('n', '<C-f>', ':NERDTreeFind<CR>', {noremap=true})

-- NETRW
vim.cmd [[
let g:netrw_banner = 0
let g:NetrwIsOpen=0

function! ToggleNetrw()
    if g:NetrwIsOpen
        let i = bufnr("$")
        while (i >= 1)
            if (getbufvar(i, "&filetype") == "netrw")
                silent exe "bwipeout " . i 
            endif
            let i-=1
        endwhile
        let g:NetrwIsOpen=0
    else
        let g:NetrwIsOpen=1
        silent Lexplore 
		silent vertical resize 30
    endif
endfunction

" Add your own mapping. For example:
noremap <silent> <leader>rw :call ToggleNetrw()<CR>
]]
-- vim.api.nvim_set_keymap('n', '<leader>rw', ':wincmd v <bar> :Ex <bar> :vertical resize 30<CR>', {noremap=true})

-- DISABLING Q
vim.api.nvim_set_keymap('n', 'Q', '', {noremap=true})

-- TERMINAL
vim.api.nvim_set_keymap('t', '<Esc>', '<c-\\><c-n>', {noremap=true})
vim.api.nvim_set_keymap('n', '<leader>t"', ':belowrigh sp | term<CR>', {noremap=true})
vim.api.nvim_set_keymap('n', '<leader>t%', ':belowright vsp | term<CR>', {noremap=true})

-- BETTER LINE MOVEMENT
vim.api.nvim_set_keymap('v', 'J', ':m \'>+1<CR>gv=gv', {noremap=true})
vim.api.nvim_set_keymap('v', 'K', ":m '<-2<CR>gv=gv", {noremap=true})
vim.api.nvim_set_keymap('v', 'H', ':\'><<CR>gv', {noremap=true})
vim.api.nvim_set_keymap('v', 'L', ':\'>><CR>gv', {noremap=true})

-- BETTER PASTE
vim.api.nvim_set_keymap('x', '<leader>p', '"_dP', {noremap=true})

-- BETTER YANK
vim.api.nvim_set_keymap('v', '<leader>y', '"+y', {noremap=true})
vim.api.nvim_set_keymap('n', '<leader>y', '"+y', {noremap=true})
vim.api.nvim_set_keymap('n', '<leader>Y', 'gg"+yG', {noremap=true})

-- BETTER DELETE
vim.api.nvim_set_keymap('n', '<leader>d', '"_d', {noremap=true})
vim.api.nvim_set_keymap('v', '<leader>d', '"_d', {noremap=true})

-- BETTER CHANGE
vim.api.nvim_set_keymap('n', '<leader>c', '"_c', {noremap=true})
vim.api.nvim_set_keymap('v', '<leader>c', '"_c', {noremap=true})
vim.api.nvim_set_keymap('n', 's', '"_s', {noremap=true})
vim.api.nvim_set_keymap('v', 's', '"_s', {noremap=true})

-- SOURCE NEOVIM
vim.env.NVIM_CONFIG_PATH = vim.fn.stdpath('config')
vim.api.nvim_set_keymap('n', '<Leader>so', ':luafile $NVIM_CONFIG_PATH/init.lua<CR>', {noremap=true})

-- BETTER UNDO
vim.api.nvim_set_keymap('i', ',', ',<c-g>u', {noremap=true})
vim.api.nvim_set_keymap('i', '.', '.<c-g>u', {noremap=true})
vim.api.nvim_set_keymap('i', '!', '!<c-g>u', {noremap=true})
vim.api.nvim_set_keymap('i', '?', '?<c-g>u', {noremap=true})

-- KEEPING CURSOR IN CENTER
vim.api.nvim_set_keymap('n', 'n', 'nzzzv', {noremap=true})
vim.api.nvim_set_keymap('n', 'N', 'Nzzzv', {noremap=true})
vim.api.nvim_set_keymap('n', 'J', 'mzJ`z', {noremap=true})

-- OPERATION PENDING WITH BRACKETS AND PARANTESES
vim.cmd [[
onoremap b i[|                            
onoremap p i(|
onoremap c i{|
]]

-- MOVE TO END OR THE START OF THE LINE
vim.cmd [[
nnoremap H ^
nnoremap L $
]]

-- GO BACK TO NORMAL MODE
vim.cmd [[
"inoremap jk <ESC>
"inoremap kj <ESC>
inoremap <C-c> <ESC>
]]

--LUASNIPPET
vim.cmd [[
imap <silent><expr> <Tab> luasnip#expand_or_jumpable() ? '<Plug>luasnip-expand-or-jump' : '<Tab>'
inoremap <silent> <S-Tab> <cmd>lua require'luasnip'.jump(-1)<Cr>

snoremap <silent> <Tab> <cmd>lua require('luasnip').jump(1)<Cr>
snoremap <silent> <S-Tab> <cmd>lua require('luasnip').jump(-1)<Cr>

imap <silent><expr> <C-E> luasnip#choice_active() ? '<Plug>luasnip-next-choice' : '<C-E>'
smap <silent><expr> <C-E> luasnip#choice_active() ? '<Plug>luasnip-next-choice' : '<C-E>'

]]


local opts = { noremap=true, silent=true }
vim.api.nvim_set_keymap('n', '<leader>vD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>vd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>vi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>sh', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>vh', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
vim.api.nvim_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
vim.api.nvim_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
vim.api.nvim_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>fo', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)
