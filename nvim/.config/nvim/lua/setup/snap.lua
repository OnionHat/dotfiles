local ok_status, snap = pcall(require, "snap")
if not ok_status then
	return
end
-- local snap = require("snap")
local prompt_reverse = true
local find_file = snap.config.file:with({
	producer = "ripgrep.file",
	prompt = "Find file",
	suffix = ":",
	reverse = prompt_reverse,
	consumer = "fzf",
})
local buffer = snap.config.file:with({
	producer = "vim.buffer",
	prompt = "Buffer",
	suffix = ":",
	reverse = prompt_reverse,
	consumer = "fzf",
})
local grep = snap.config.vimgrep:with({
	prompt = "Grep",
	suffix = ":",
	limit = 50000,
	reverse = prompt_reverse,
})
local old_files = snap.config.file:with({
	producer = "vim.oldfile",
	prompt = "Grep",
	suffix = ":",
	limit = 50000,
	reverse = prompt_reverse,
	consumer = "fzf",
})

snap.maps({
	{ "<Leader><Leader>", find_file({}) },
	{ "<Leader>fb", buffer({}) },
	{ "<Leader>fg", grep({}) },
	{ "<Leader>ff", old_files({}) },
})

-- https://github.com/yujinyuz/dotfiles/blob/master/nvim/.config/nvim/lua/config/snap.lua
