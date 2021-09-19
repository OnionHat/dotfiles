set exrc
set relativenumber
set nu
set nohlsearch
set hidden
set noerrorbells
syntax on
filetype plugin on

set mouse=a

set tabstop=4 softtabstop=4
set shiftwidth=4

set expandtab
set smartindent
set breakindent
set breakindentopt=shift:4,min:40,sbr
set showbreak=>>

set noswapfile
set nobackup
set undodir=~/.vim/undodir
set undofile

set incsearch
set termguicolors
set scrolloff=8
set noshowmode
set completeopt=menuone,noinsert,noselect

set colorcolumn=80
set signcolumn=yes

set cmdheight=2
set clipboard=unnamedplus

set cursorline
"set cursorcolumn

" Set cursor line color on visual mode
highlight Visual cterm=NONE ctermbg=236 ctermfg=NONE guibg=Grey40

highlight LineNr cterm=none ctermfg=240 guifg=#2b506e guibg=#000000

augroup BgHighlight
    autocmd!
    autocmd WinEnter * set cul
    autocmd WinLeave * set nocul
augroup END

if &term =~ "screen"
    autocmd BufEnter * if bufname("") !~ "^?[A-Za-z0-9?]*://" | silent! exe '!echo -n "\ek[`hostname`:`basename $PWD`/`basename %`]\e\\"' | endif
    autocmd VimLeave * silent!  exe '!echo -n "\ek[`hostname`:`basename $PWD`]\e\\"'
endif

call plug#begin('~/.config/nvim/plugged')
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/nerdcommenter'

Plug 'lambdalisue/suda.vim'

Plug 'hoob3rt/lualine.nvim'
Plug 'ryanoasis/vim-devicons'

Plug 'tpope/vim-surround'

Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'

Plug 'mbbill/undotree'

"Plug 'gruvbox-community/gruvbox'
"Plug 'morhetz/gruvbox'
Plug 'altercation/vim-colors-solarized'

"Plug 'christoomey/vim-tmux-navigator'

Plug 'neovim/nvim-lspconfig'
Plug 'kabouzeid/nvim-lspinstall'
Plug 'ms-jpq/coq_nvim', {'branch': 'coq'}
Plug 'ms-jpq/coq.artifacts', {'branch': 'artifacts'}

Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/nvim-cmp'
Plug 'hrsh7th/cmp-path'
Plug 'andersevenrud/compe-tmux', {'branch': 'cmp'}
Plug 'ray-x/cmp-treesitter'
Plug 'hrsh7th/cmp-vsnip'
Plug 'hrsh7th/vim-vsnip'

Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'nvim-treesitter/nvim-treesitter-refactor'


Plug 'folke/todo-comments.nvim'
call plug#end()

set completeopt=menuone,noselect,noinsert

lua require('lualine').setup{ options = {theme = 'solarized'} }

lua require'nvim-treesitter.configs'.setup { highlight = { enable = true }, incremental_selection = { enable = true }, textobjects = { enable = true }}

let mapleader= " "

map <F7> gg=G<C-o><C-o>

" true color
if exists("&termguicolors") && exists("&winblend")
    syntax enable
    set termguicolors
    set winblend=0
    set wildoptions=pum
    set pumblend=5
    set background=dark
    " Use NeoSolarized
    let g:neosolarized_termtrans=1
    runtime ./colors/NeoSolarized.vim
    colorscheme NeoSolarized
endif

"let g:Hexokinase_highlighters = ['backgroundfull']

nnoremap <leader>n :NERDTreeFocus<CR>
nnoremap <C-n> :NERDTree<CR>
nnoremap <C-t> :NERDTreeToggle<CR>
nnoremap <C-f> :NERDTreeFind<CR>

" Undotree
nnoremap <F5> :UndotreeToggle<CR>

" Telescope
nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>

"disabling keys for better or worse
nnoremap Q <Nop>

nnoremap <Down> :echo "No down for you!"<CR>
vnoremap <Down> <C-u>:echo "No down for you!"<CR>
inoremap <Down> <C-o>:echo "No down for you!"<CR>

nnoremap <Up> :echo "No up for you!"<CR>
vnoremap <Up> <C-u>:echo "No up for you!"<CR>
inoremap <Up> <C-o>:echo "No up for you!"<CR>

nnoremap <Left> :echo "No left for you!"<CR>
vnoremap <Left> <C-u>:echo "No left for you!"<CR>
inoremap <Left> <C-o>:echo "No left for you!"<CR>

nnoremap <Right> :echo "No right for you!"<CR>
vnoremap <Right> <C-u>:echo "No right for you!"<CR>
inoremap <Right> <C-o>:echo "No Right for you!"<CR>

lua << EOF
local lsp = require "lspconfig"
local cmp = require'cmp'

cmp.setup({
    snippet = 
    {
        expand = function(args)
        -- For `vsnip` user.
        vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` user.

        -- For `luasnip` user.
        -- require('luasnip').lsp_expand(args.body)

        -- For `ultisnips` user.
        -- vim.fn["vsnip#anonymous"](args.body)
        end,
    },
    mapping = 
    {
        ['<C-d>']       = cmp.mapping.scroll_docs(-4),
        ['<C-f>']       = cmp.mapping.scroll_docs(4),
        ['<C-Space>']   = cmp.mapping.complete(),
        ['<C-e>']       = cmp.mapping.close(),
        ['<CR>']        = cmp.mapping.confirm({ select = true }),
        ['<C-p>']       = cmp.mapping.select_prev_item(),
        ['<C-n>']       = cmp.mapping.select_next_item(),
    },
    sources = 
    {
        { name = 'nvim_lsp' },
        { name = 'path' },
        { name = 'tmux' },
        { name = 'treesitter' },
        -- For vsnip user.
        { name = 'vsnip' },
        -- For luasnip user.
        -- { name = 'luasnip' },
        -- For ultisnips user.
        -- { name = 'ultisnips' },
        { name = 'buffer' },
    }
})

-- Setup lspconfig.
-- require('lspconfig').clangd.setup {
--     capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
-- }
require('lspconfig').ccls.setup {
    capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
}
require('lspconfig').pyright.setup {
    capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
}
require('lspconfig').vimls.setup {
    capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
}
require('lspconfig').bashls.setup {
    capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
}

require("todo-comments").setup{
    signs = false, -- show icons in the signs column
    sign_priority = 8, -- sign priority
    -- keywords recognized as todo comments
    keywords = {
    FIX = {
      icon = "", -- icon used for the sign, and in search results
      color = "error", -- can be a hex color, or a named color (see below)
      alt = { "FIXME", "BUG", "FIXIT", "ISSUE" }, -- a set of other keywords that all map to this FIX keywords
      -- signs = false, -- configure signs for some keywords individually
    },
    TODO = { icon = "", color = "#eb4034" },
    NOTE = { icon = "", color = "#31bf2c", alt = { "INFO" } },
    },
    merge_keywords = true, -- when true, custom keywords will be merged with the defaults
    -- highlighting of the line containing the todo comment
    -- * before: highlights before the keyword (typically comment characters)
    -- * keyword: highlights of the keyword
    -- * after: highlights after the keyword (todo text)
    highlight = {
    before = "", -- "fg" or "bg" or empty
    keyword = "fg", -- "fg", "bg", "wide" or empty. (wide is the same as bg, but will also highlight surrounding characters)
    after = "", -- "fg" or "bg" or empty
    pattern = [[.*<(KEYWORDS)\s*:]], -- pattern or table of patterns, used for highlightng (vim regex)
    comments_only = true, -- uses treesitter to match keywords in comments only
    max_line_len = 400, -- ignore lines longer than this
    exclude = {}, -- list of file types to exclude highlighting
    },
    -- list of named colors where we try to extract the guifg from the
    -- list of hilight groups or use the hex color if hl not found as a fallback
    colors = {
    error = { "LspDiagnosticsDefaultError", "ErrorMsg", "#DC2626" },
    warning = { "LspDiagnosticsDefaultWarning", "WarningMsg", "#FBBF24" }, 
    info = { "LspDiagnosticsDefaultInformation", "#eb4034" },
    hint = { "LspDiagnosticsDefaultHint", "#10B981" },
    default = { "Identifier", "#7C3AED" },
    },
    search = {
    command = "rg",
    args = {
      "--color=never",
      "--no-heading",
      "--with-filename",
      "--line-number",
      "--column",
    },
    -- regex that will be used to match keywords.
    -- don't replace the (KEYWORDS) placeholder
    pattern = [[\b(KEYWORDS):]], -- ripgrep regex
    -- pattern = [[\b(KEYWORDS)\b]], -- match without the extra colon. You'll likely get false positives
    },
}



-- vim.g.coq_settings = {
--     auto_start = true,
--     clients = {
--         tabnine = {
--         enabled = true,
--         }
--     },
-- }
-- local coq = require "coq" -- add this
-- 
-- lsp.clangd.setup{coq.lsp_ensure_capabilities()}
-- lsp.pyright.setup{coq.lsp_ensure_capabilities({on_attach = on_attach_callback,})}
-- --lsp.pylsp.setup{coq.lsp_ensure_capabilities({on_attach = on_attach_callback,})}
-- lsp.vimls.setup{coq.lsp_ensure_capabilities()}
-- lsp.bashls.setup{coq.lsp_ensure_capabilities()}
EOF
