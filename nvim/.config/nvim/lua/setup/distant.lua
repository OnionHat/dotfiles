local ok_status, distant = pcall(require, "distant")
if not ok_status then
	return
end
distant.setup({
	-- Applies Chip's personal settings to every machine you connect to
	--
	-- 1. Ensures that distant servers terminate with no connections
	-- 2. Provides navigation bindings for remote directories
	-- 3. Provides keybinding to jump into a remote file's parent directory
	["*"] = require("distant.settings").chip_default(),
	{
		mode = "ssh",
		user = "suleymab",
	},
	["login.ifi.uio.no"] = {
		mode = "ssh",
		ssh = {
			user = "suleymab",
		},
	},
})
