local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
	fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
	vim.cmd 'packadd packer.nvim'
end

vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerCompile
  augroup end
]])

return require('packer').startup(function(use)
	-- Packer can manage itself
	use 'wbthomason/packer.nvim'

	-- TELESCOPE
	use 'nvim-telescope/telescope.nvim'
	use 'nvim-lua/plenary.nvim'
	use 'nvim-lua/popup.nvim'

	use {
		'neoclide/coc.nvim'
	}

	-- LSP
	use 'neovim/nvim-lspconfig'



	-- TREESITTER
	use {
		'nvim-treesitter/nvim-treesitter',
		run = ':TSUpdate',
		requires =  'nvim-treesitter/nvim-treesitter-refactor',
	}

	-- COMPLETION
	use {
		'hrsh7th/nvim-cmp',
		requires = {
			'hrsh7th/cmp-nvim-lsp',
			'hrsh7th/cmp-buffer',
			'hrsh7th/cmp-path',
			'hrsh7th/cmp-nvim-lua',
			'saadparwaiz1/cmp_luasnip',
			'onsails/lspkind-nvim'
		}
	}

	-- SNIPPET
	use 'L3MON4D3/LuaSnip'

	-- COLOR SCHEME
	use 'dracula/vim'
	use 'gosukiwi/vim-atom-dark'
	use 'joshdick/onedark.vim'
	use 'gruvbox-community/gruvbox'
	use 'nanotech/jellybeans.vim'
	use 'kristijanhusak/vim-hybrid-material'
	--use 'altercation/vim-colors-solarized'
	use 'sainnhe/everforest'
	use 'caksoylar/vim-mysticaltutor'
	use 'arcticicestudio/nord-vim'
	use 'NLKNguyen/papercolor-theme'
	use 'folke/tokyonight.nvim'

	-- TPOPE
	use 'tpope/vim-surround'
	-- use 'tpope/vim-commentary'
	use 'tpope/vim-projectionist'
	use 'tpope/vim-vinegar'

	use {
		'numToStr/Comment.nvim',
		config = function()
			require('Comment').setup()
		end
	}

	-- FOLKE
	use {
		"folke/todo-comments.nvim",
		requires = "nvim-lua/plenary.nvim",
		config = function()
			require("todo-comments").setup {
				signs = false,
				highlight = {
					keyword = "fg", -- "fg", "bg", "wide" or empty. (wide is the same as bg, but will also highlight surrounding characters)
					after = "", -- "fg" or "bg" or empty
				},
				colors = {
					error = { "LspDiagnosticsDefaultError", "ErrorMsg", "#DC2626" },
					warning = { "LspDiagnosticsDefaultWarning", "WarningMsg", "#FBBF24" },
					info = { "LspDiagnosticsDefaultInformation", "#2563EB" },
					hint = { "LspDiagnosticsDefaultHint", "#10B981" },
					default = { "Identifier", "#7C3AED" },
				},
			}
		end
	}
	-- use 'folke/which-key.nvim'

	-- DAP
	-- use 'mfussenegger/nvim-dap'
	-- use 'rcarriga/nvim-dap-ui'

	-- STATUSLINE
	use { 'hoob3rt/lualine.nvim', requires = 'ryanoasis/vim-devicons', }


	use 'lambdalisue/suda.vim'
	use 'mbbill/undotree'
	use 'christoomey/vim-tmux-navigator'

	-- use {
	-- 	'rrethy/vim-hexokinase',
	-- 	run = 'make hexokinase',
	-- 	-- config = function()
	-- 	-- 	vim.cmd 'call hexokinase#v2#scraper#off()'
	-- 	--end
	-- }

	use { 'windwp/nvim-autopairs'}

	use {'iamcco/markdown-preview.nvim', config = "vim.call('mkdp#util#install')"}
	use {
		'sudormrfbin/cheatsheet.nvim',
		requires = {
			{'nvim-telescope/telescope.nvim'},
			{'nvim-lua/popup.nvim'},
			{'nvim-lua/plenary.nvim'},
		}
	}
	use {
		'Shougo/echodoc.vim',
		config = function()
			vim.cmd [[
			set noshowmode
			let g:echodoc_enable_at_startup = 1
			]]
		end
	}

end)
