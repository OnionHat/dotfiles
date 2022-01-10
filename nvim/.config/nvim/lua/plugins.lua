local fn = vim.fn
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
	fn.system({ "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path })
	vim.cmd("packadd packer.nvim")
end

vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerCompile
  augroup end
]])

local get_setup = function(name)
	return string.format("require('setup/%s')", name)
end

return require("packer").startup(function(use)
	-- Packer can manage itself
	use("wbthomason/packer.nvim")

	-- TELESCOPE{{{
	use({
		"nvim-telescope/telescope.nvim",
		requires = {
			"nvim-lua/plenary.nvim",
			"nvim-lua/popup.nvim",
			"nvim-telescope/telescope-media-files.nvim",
			"tami5/sqlite.lua",
			"nvim-telescope/telescope-cheat.nvim",
			"nvim-telescope/telescope-file-browser.nvim",
		},
		config = get_setup("telescope"),
	})
	use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" }) --}}}

	-- LSP{{{
	use({
		"neovim/nvim-lspconfig",
		config = get_setup("lsp"),
	})

	use({
		"jose-elias-alvarez/null-ls.nvim",
		config = get_setup("null-ls"),
	})

	use({
		"tami5/lspsaga.nvim",
		config = get_setup("lspsaga"),
	})

	use({
		"ThePrimeagen/refactoring.nvim",
		requires = {
			{ "nvim-lua/plenary.nvim" },
			{ "nvim-treesitter/nvim-treesitter" },
		},
		config = get_setup("refactoring"),
	})
	use({
		"ray-x/lsp_signature.nvim",
		config = get_setup("signature"),
	})
	use({
		"simrat39/symbols-outline.nvim",
        config = get_setup("symbols-outline")
	})
	--}}}

	-- TREESITTER{{{
	use({
		"nvim-treesitter/nvim-treesitter",
		run = ":TSUpdate",
		requires = "nvim-treesitter/nvim-treesitter-refactor",
		config = get_setup("treesitter"),
	}) --}}}

	-- COMPLETION{{{
	use({
		"hrsh7th/nvim-cmp",
		requires = {
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-buffer",
			"hrsh7th/cmp-path",
			"hrsh7th/cmp-nvim-lua",
			"saadparwaiz1/cmp_luasnip",
			"onsails/lspkind-nvim",
			"hrsh7th/cmp-nvim-lsp-signature-help",
		},
		config = get_setup("completion"),
	})
	use("L3MON4D3/LuaSnip") --}}}

	-- COLOR SCHEME{{{
	use({
		"rktjmp/lush.nvim",
		requires = "metalelf0/jellybeans-nvim",
	})
	use("lunarvim/darkplus.nvim")
	-- use 'dracula/vim'
	-- use 'gosukiwi/vim-atom-dark'
	-- use 'joshdick/onedark.vim'
	-- use 'gruvbox-community/gruvbox'
	-- use 'nanotech/jellybeans.vim'
	-- use 'kristijanhusak/vim-hybrid-material'
	--use 'altercation/vim-colors-solarized'
	-- use 'sainnhe/everforest'
	-- use 'caksoylar/vim-mysticaltutor'
	-- use 'arcticicestudio/nord-vim'
	-- use 'NLKNguyen/papercolor-theme'
    use 'jacoborus/tender.vim'
	use 'folke/tokyonight.nvim'
    --}}}

	-- TPOPE{{{
	use("tpope/vim-surround")
	use({
		"tpope/vim-projectionist",
		config = get_setup("projectionist"),
	})
	use("tpope/vim-dispatch")
	use("tpope/vim-vinegar") --}}}

	-- COMMENTING{{{
	use({
		"numToStr/Comment.nvim",
		config = get_setup("comment"),
	}) --}}}

	-- FOLKE{{{
	use({
		"folke/todo-comments.nvim",
		requires = "nvim-lua/plenary.nvim",
		config = get_setup("todo-comments"),
	})
	-- use({
	-- 	"folke/which-key.nvim",
	-- 	config = get_setup("which-key"),
	-- })
	use({
		"folke/trouble.nvim",
		requires = "kyazdani42/nvim-web-devicons",
		config = get_setup("trouble"),
	}) --}}}

	-- DAP{{{
	use({
		"mfussenegger/nvim-dap",
		requires = "mfussenegger/nvim-dap-python",
		config = get_setup("nvim-dap"),
	})
	use("rcarriga/nvim-dap-ui") --}}}

	-- STATUSLINE{{{
	use({
		"hoob3rt/lualine.nvim",
		requires = "ryanoasis/vim-devicons",
		config = get_setup("lualine"),
	})
	use({
		"SmiteshP/nvim-gps",
		requires = "nvim-treesitter/nvim-treesitter",
		config = get_setup("nvim-gps"),
	}) --}}}

	-- MISC{{{
	use("lambdalisue/suda.vim")
	use("mbbill/undotree")
	use("christoomey/vim-tmux-navigator")
	use({
		"akinsho/toggleterm.nvim",
		config = get_setup("toggleterm"),
		after = "vim-tmux-navigator",
	})
	--}}}

	-- SHOW COLORS{{{
	use({
		"rrethy/vim-hexokinase",
		run = "make hexokinase",
	}) --}}}

	-- AUTOPAIRS{{{
	use({
		"windwp/nvim-autopairs",
		config = get_setup("autopairs"),
	}) --}}}

	-- MARKDOWN{{{
	use({ "iamcco/markdown-preview.nvim", config = "vim.call('mkdp#util#install')" }) --}}}

	-- PASTING REGISTER{{{
	use("tversteeg/registers.nvim") --}}}

	-- INDENT GUIDE{{{
	use({ "Yggdroot/indentLine" }) --}}}

	-- GIT SIGNS{{{
	use({
		"lewis6991/gitsigns.nvim",
		config = get_setup("gitsigns"),
	}) --}}}

	-- RUNNING PROGRAMS{{{
	use("skywind3000/asyncrun.vim") --}}}

	-- PYTHON{{{
	use({
		"bps/vim-textobj-python",
		ft = { "python" },
		requires = "kana/vim-textobj-user",
		config = get_setup("textobj-python"),
	})
	use({
		"hanschen/vim-ipython-cell",
		-- ft = { "python" },
		requires = "jpalardy/vim-slime",
		config = get_setup("vim-python-cell"),
	}) --}}}

	-- MAYBEEEEEE
	-- use("AckslD/nvim-revJ.lua")  -- https://github.com/AckslD/nvim-revJ.lua Reverse J
	-- use("eddiebergman/nvim-treesitter-pyfold")   -- https://github.com/eddiebergman/nvim-treesitter-pyfold Better folding I guess
end)
