-- DISABLING Q
vim.api.nvim_set_keymap('n', 'Q', '', {noremap=true})

-- TERMINAL
vim.api.nvim_set_keymap('t', '<Esc>', '<c-\\><c-n>', {noremap=true})
vim.api.nvim_set_keymap('n', '<leader>t"', ':belowrigh sp | term<CR>', {noremap=true})
vim.api.nvim_set_keymap('n', '<leader>t%', ':belowright vsp | term<CR>', {noremap=true})
vim.api.nvim_set_keymap('n', '<leader>tn', ':term<CR>', {noremap=true})

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
function _G.ReloadConfig()
  for name,_ in pairs(package.loaded) do
    if name:match('^cnull') then
      package.loaded[name] = nil
    end
  end

  dofile(vim.env.MYVIMRC)
end
vim.env.NVIM_CONFIG_PATH = vim.fn.stdpath('config')
vim.api.nvim_set_keymap('n', '<Leader>lo', ':luafile $NVIM_CONFIG_PATH/init.lua<CR>', {noremap=true})

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
onoremap b i]             
onoremap p i(|
onoremap B i{|
]]

-- MOVE TO END OR THE START OF THE LINE
vim.cmd [[
nnoremap H ^
nnoremap L $
]]

-- GO BACK TO NORMAL MODE
vim.cmd [[
inoremap <C-c> <ESC>
]]

-- BUFFER

vim.cmd [[
nnoremap <leader>bd :bd<CR>
nnoremap <leader>bD :bd!<CR>
nnoremap <leader>bp :bprev<CR>
nnoremap <leader>bn :bnext<CR>
]]

-- SAVE
vim.cmd('nnoremap <silent> <C-s> :w<CR>')
vim.cmd('inoremap <silent> <C-s> <Esc>:w<Cr>a')

-- SELECT ALL
vim.cmd('nnoremap <silent> <C-a> ggVG')

-- TMUX
vim.cmd[[
noremap <C-A-j> :resize +5<CR>
noremap <C-A-k> :resize -5<CR>
noremap <C-A-h> :vertical:resize -5<CR>
noremap <C-A-l> :vertical:resize +5<CR>
]]

-- INDENT
-- vim.cmd[[
-- inoremap <CR> <CR>x<BS>
-- nnoremap o ox<BS>
-- nnoremap O Ox<BS>
-- ]]
