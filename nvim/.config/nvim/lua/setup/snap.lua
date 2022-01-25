-- local ok_status, snap = pcall(require, "snap")
-- if ok_status then
-- 	return
-- end
local snap = require'snap'


snap.maps {
  {"<Leader><Leader>", snap.config.file {producer = "ripgrep.file"}},
  {"<Leader>fb", snap.config.file {producer = "vim.buffer"}},
  {"<Leader>fo", snap.config.file {producer = "vim.oldfile"}},
  {"<Leader>ff", snap.config.vimgrep {}},
}


-- https://github.com/yujinyuz/dotfiles/blob/master/nvim/.config/nvim/lua/config/snap.lua
