local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
	fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
	vim.cmd 'packadd packer.nvim'
end

return require('packer').startup(function(use)
	-- Packer can manage itself
	use 'wbthomason/packer.nvim'

	-- TELESCOPE
	use {
		'nvim-telescope/telescope.nvim',
		requires = { {'nvim-lua/plenary.nvim'} }
	}
	use 'nvim-lua/popup.nvim'

	-- LSP
	use {
		'neovim/nvim-lspconfig',
		config = function()
			require('utils.nvim-lsp')
		end
	}

	-- TREESITTER
	use {
		'nvim-treesitter/nvim-treesitter',
		run = ':TSUpdate',
		requires =  'nvim-treesitter/nvim-treesitter-refactor',
		config = function()
			require('utils.nvim-treesitter')
		end
	}

	-- COMPLETION
	use {
		'hrsh7th/cmp-nvim-lsp',
		requires = {
			'hrsh7th/cmp-nvim-lsp',
			'hrsh7th/cmp-buffer',
			'hrsh7th/nvim-cmp',
			'hrsh7th/cmp-path',
			'andersevenrud/compe-tmux', branch = 'cmp',
			'ray-x/cmp-treesitter',
			'hrsh7th/cmp-vsnip',
			'hrsh7th/vim-vsnip'
		},
		config = function()
			require('utils.nvim-cmp')
		end
	}

	-- COLOR SCHEME
	use 'gruvbox-community/gruvbox'
	use 'altercation/vim-colors-solarized'
	use 'sainnhe/everforest'
	use 'caksoylar/vim-mysticaltutor'
	use 'nanotech/jellybeans.vim'

	-- STATUSLINE
	use {
		'hoob3rt/lualine.nvim',
		requires = 'ryanoasis/vim-devicons',
		config = function()
			require('lualine').setup{ options = {theme = 'jellybeans'} }
		end
	}

	use 'lambdalisue/suda.vim'

	use 'tpope/vim-surround'

	use 'mbbill/undotree'

	-- NERD STUFF
	use 'scrooloose/nerdcommenter'
	use 'scrooloose/nerdtree'

	use 'christoomey/vim-tmux-navigator'

	use {
		"folke/todo-comments.nvim",
		requires = "nvim-lua/plenary.nvim",
		config = function()
			require("todo-comments").setup {
				signs = false,
				highlight = {
					before = "", -- "fg" or "bg" or empty
					keyword = "fg", -- "fg", "bg", "wide" or empty. (wide is the same as bg, but will also highlight surrounding characters)
					after = "", -- "fg" or "bg" or empty
					pattern = [[.*<(KEYWORDS)\s*:]], -- pattern or table of patterns, used for highlightng (vim regex)
					comments_only = true, -- uses treesitter to match keywords in comments only
					max_line_len = 400, -- ignore lines longer than this
					exclude = {}, -- list of file types to exclude highlighting
				},
			}
		end
	}

	use { 'rrethy/vim-hexokinase', run = 'make hexokinase' }
	use 'b3nj5m1n/kommentary'

	use {"akinsho/toggleterm.nvim"}
end)
