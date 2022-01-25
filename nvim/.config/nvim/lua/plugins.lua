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

	-- Maybe a better file search
	use({ "camspiers/snap", rocks = { "fzy" } })

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
	use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" })
	--}}}

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

		"ray-x/lsp_signature.nvim",
		config = get_setup("signature"),
	})

	--}}}

	-- TREESITTER{{{
	use({
		"nvim-treesitter/nvim-treesitter",
		run = ":TSUpdate",
		requires = "nvim-treesitter/nvim-treesitter-refactor",
		config = get_setup("treesitter"),
	})
	--}}}

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
			"L3MON4D3/LuaSnip",
		},
		config = get_setup("completion"),
	})
	--}}}

	-- COLORS {{{
	use({ -- Colorscheme
		"gruvbox-community/gruvbox",
		"nanotech/jellybeans.vim",
		"folke/tokyonight.nvim",
		-- "lunarvim/darkplus.nvim",
		-- "dracula/vim",
		-- "gosukiwi/vim-atom-dark",
		-- "joshdick/onedark.vim",
		-- "altercation/vim-colors-solarized",
		-- "sainnhe/everforest",
		-- "caksoylar/vim-mysticaltutor",
		-- "arcticicestudio/nord-vim",
		-- "NLKNguyen/papercolor-theme",
		-- "jacoborus/tender.vim",
		-- "drewtempelmeyer/palenight.vim",
		-- "p00f/alabaster_dark.nvim",
		-- "rebelot/kanagawa.nvim",
		-- "ozkanonur/nimda.vim",
		-- "sainnhe/gruvbox-material",

		requires = "rktjmp/lush.nvim",
	})

	use({ "rrethy/vim-hexokinase", run = "make hexokinase" }) -- Shows the color

	use("p00f/nvim-ts-rainbow") -- Colors the curly brackets and like
	--}}}

	-- TPOPE{{{
	use("tpope/vim-surround", "tpope/vim-dispatch")
	--}}}

	-- COMMENTING{{{
	use({
		"numToStr/Comment.nvim",
		config = get_setup("comment"),
	})
	--}}}

	-- FOLKE{{{
	use({
		"folke/todo-comments.nvim",
		requires = "nvim-lua/plenary.nvim",
		config = get_setup("todo-comments"),
	})
	use({
		"folke/trouble.nvim",
		requires = "kyazdani42/nvim-web-devicons",
		config = get_setup("trouble"),
	})
	use({
		"folke/lsp-colors.nvim",
		config = require("lsp-colors").setup({}),
	})
	--}}}

	-- DAP{{{
	use({
		"mfussenegger/nvim-dap",
		"rcarriga/nvim-dap-ui",
		requires = "mfussenegger/nvim-dap-python",
		config = get_setup("nvim-dap"),
	})
	--}}}

	-- STATUSLINE{{{
	use({
		"windwp/windline.nvim",
		config = get_setup("windline"),

		"SmiteshP/nvim-gps", -- Shows function parameter on statusbar
		requires = "nvim-treesitter/nvim-treesitter",
		config = get_setup("nvim-gps"),
	})
	--}}}

	-- MAKE{{{
	use("neomake/neomake", "skywind3000/asyncrun.vim")
	--}}}

	-- MISC{{{
	use({
		"lambdalisue/suda.vim", -- Sudo wirte and edit
		"mbbill/undotree", -- Undotree
		"christoomey/vim-tmux-navigator", -- Use same bindings for navigating vim splits and tmux panes
		"elihunter173/dirbuf.nvim", -- Better file editing
	})

	use({
		"akinsho/toggleterm.nvim", -- Intigrated terminal
		config = get_setup("toggleterm"),
		after = "vim-tmux-navigator",
	})

	use({
		"windwp/nvim-autopairs",
		config = get_setup("autopairs"),
	})
	-- }}}

	-- LANGUAGE SPECIFIC {{{
	-- Markdown
	use({ "iamcco/markdown-preview.nvim", config = "vim.call('mkdp#util#install')" })

	-- Pyhon
	use({
		"bps/vim-textobj-python",
		ft = { "python" },
		requires = "kana/vim-textobj-user",
		config = get_setup("textobj-python"),
	})

	-- Java
	use("mfussenegger/nvim-jdtls")
	--}}}

	-- PASTING REGISTER{{{
	use("tversteeg/registers.nvim") --}}}

	-- INDENT GUIDE{{{
	use({ "Yggdroot/indentLine" }) --}}}

	-- GIT SIGNS{{{
	use({
		"lewis6991/gitsigns.nvim",
		config = get_setup("gitsigns"),
	}) --}}}

	-- REMOTE{{{
	use({
		"chipsenkbeil/distant.nvim",
		config = get_setup("distant"),
	})
	--}}}

	-- MAYBEEEEEE
	-- use("AckslD/nvim-revJ.lua")  -- https://github.com/AckslD/nvim-revJ.lua Reverse J
	-- use("eddiebergman/nvim-treesitter-pyfold")   -- https://github.com/eddiebergman/nvim-treesitter-pyfold Better folding I guess
end)
