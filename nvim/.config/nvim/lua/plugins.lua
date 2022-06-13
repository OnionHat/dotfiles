local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
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
	-- 		"nvim-telescope/telescope-media-files.nvim",
	-- 		"tami5/sqlite.lua",
	-- 		"nvim-telescope/telescope-cheat.nvim",
	-- 		"nvim-telescope/telescope-file-browser.nvim",
		},
		config = get_setup("telescope"),
	})
	use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" })
	--}}}

	-- LSP{{{
	use({ "neovim/nvim-lspconfig", config = get_setup("lsp") })

	use({ "jose-elias-alvarez/null-ls.nvim", config = get_setup("null-ls") })

	use({ "ray-x/lsp_signature.nvim", config = get_setup("signature") })

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
			"onsails/lspkind-nvim",
			"saadparwaiz1/cmp_luasnip",
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
	-- use("p00f/nvim-ts-rainbow") -- Colors the curly brackets and like
	--}}}

	-- TPOPE{{{
	use("tpope/vim-surround")
	--}}}

	-- COMMENTING{{{
	use({ "numToStr/Comment.nvim", config = get_setup("comment") })
	--}}}

	-- STATUSLINE{{{
	use({ "windwp/windline.nvim", config = get_setup("windline") }) -- Statusline
	--}}}

	-- MISC{{{
	use({
		"lambdalisue/suda.vim", -- Sudo wirte and edit
		"mbbill/undotree", -- Undotree
		"christoomey/vim-tmux-navigator", -- Use same bindings for navigating vim splits and tmux panes
        "mboughaba/i3config.vim",
	})
	use({ "windwp/nvim-autopairs", config = get_setup("autopairs") })
	-- }}}

    if packer_bootstrap then
        require('packer').sync()
    end
end)
