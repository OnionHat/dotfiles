-- BUFFER-LOCAL OPTIONS
vim.o.tabstop           = 4
vim.o.softtabstop		= 0
vim.o.shiftwidth        = 0
vim.o.expandtab       	= false
vim.o.smartindent       = true


-- WINDOW-LOCAL OPTIONS
vim.o.relativenumber   = true
vim.o.number           = true
vim.o.foldmethod       = "marker"
--vim.wo.breakindent      = true
--vim.wo.breakindentopt   = "min:40,shift:4,sbr"
--vim.wo.showbreak        = ">>"
vim.o.colorcolumn      = "80"
vim.o.cursorline       = true
--vim.wo.cursorcolumn     = true


-- GLOBAL OPTIONS
vim.o.hlsearch         = false
vim.o.hidden           = true
vim.o.errorbells       = false
vim.o.mouse            = "nv"
vim.o.backup           = false
vim.o.swapfile         = false
--vim.o.undodir		= ".undo/"
vim.o.undofile         = true
vim.o.incsearch        = true
vim.o.termguicolors    = true
vim.o.scrolloff        = 8
vim.o.showmode         = true
vim.o.completeopt      = "menuone,noinsert,noselect"
vim.o.cmdheight        = 2
vim.o.clipboard        = "unnamedplus"
vim.o.keywordprg       = ":man"

vim.g.mapleader	= " "

--vim.cmd 'filetype plugin indent off'
--vim.cmd 'syntax off'
--let mapleader= " "

