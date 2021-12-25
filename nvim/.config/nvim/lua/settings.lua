-- BUFFER-LOCAL OPTIONS
vim.opt.tabstop			= 4
vim.opt.softtabstop		= 0
vim.opt.shiftwidth		= 0
vim.opt.expandtab		= false
vim.opt.smartindent		= true



-- WINDOW-LOCAL OPTIONS
vim.opt.relativenumber	= true
vim.opt.number			= true
vim.opt.foldmethod		= "marker"
--vim..opt.breakindent	= true
--vim..opt.breakindentopt	= "min:40,shift:4,sbr"
--vim..opt.showbreak		= ">>"
vim.opt.colorcolumn		= "80"
vim.opt.cursorline		= true
--vim..opt.cursorcolumn	= true


-- GLOBAL OPTIONS
vim.opt.hlsearch		= false
vim.opt.hidden			= true
vim.opt.errorbells		= false
vim.opt.mouse			= "nv"
vim.opt.backup			= false
vim.opt.swapfile		= false
-- vim.opt.undodir		= "~/.cache/undodir"
vim.opt.undofile		= true
vim.opt.incsearch		= true
vim.opt.termguicolors	= true
vim.opt.scrolloff		= 8
vim.opt.showmode		= true
vim.opt.completeopt		= "menuone,noinsert,noselect"
vim.opt.cmdheight		= 2
vim.opt.clipboard		= "unnamedplus"
vim.opt.keywordprg		= ":help"
vim.opt.autoread		= true

vim.g.mapleader	= " "

--vim.cmd 'filetype plugin indent off'
--vim.cmd 'syntax off'
-- vim.cmd[[
-- autocmd BufEnter * lcd %:p:h
-- ]]
