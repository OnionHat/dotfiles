local ok_status, dap = pcall(require, "dap")
if not ok_status then
	return
end

vim.fn.sign_define("DapBreakpoint", {
	text = "",
	texthl = "LspDiagnosticsFloatingError",
	linehl = "",
	numhl = "",
})
vim.fn.sign_define("DapBreakpointRejected", {
	text = "",
	texthl = "LspDiagnosticsSignHint",
	linehl = "",
	numhl = "",
})
vim.fn.sign_define("DapStopped", {
	text = "",
	texthl = "LspDiagnosticsFloatingInfor",
	linehl = "DiagnosticUnderlineError",
	numhl = "LspDiagnosticsFloatingInfor",
})

-- PYTHON
local dap_python_ok_status, dap_python = pcall(require, "dap-python")
if dap_python_ok_status then
	dap_python.setup("/usr/bin/python")
end

local wk_ok_status, wk = pcall(require, "which-key")
if wk_ok_status then
	wk.register({
		x = {
			name = "Debug",
			t = { "<cmd>lua require'dap'.toggle_breakpoint()<cr>", "Toggle Breakpoint" },
            T = { "<cmd>lua require'dap'.set_breakpoint(vim.fn.input('Breakpoint condition: '))<CR>", "Set Breakpoint"},
			c = { "<cmd>lua require'dap'.continue()<cr>", "Continue" },
			b = { "<cmd>lua require'dap'.step_back()<cr>", "Step Back" },
			i = { "<cmd>lua require'dap'.step_into()<cr>", "Step Into" },
			o = { "<cmd>lua require'dap'.step_over()<cr>", "Step Over" },
			u = { "<cmd>lua require'dap'.step_out()<cr>", "Step Out" },
			C = { "<cmd>lua require'dap'.run_to_cursor()<cr>", "Run To Cursor" },
			d = { "<cmd>lua require'dap'.disconnect()<cr>", "Disconnect" },
			g = { "<cmd>lua require'dap'.session()<cr>", "Get Session" },
			p = { "<cmd>lua require'dap'.pause.toggle()<cr>", "Pause" },
			r = { "<cmd>lua require'dap'.repl.toggle()<cr>", "Toggle Repl" },
			s = { "<cmd>lua require'dap'.continue()<cr>", "Start" },
			q = { "<cmd>lua require'dap'.close()<cr>", "Quit" },
			l = { "<cmd>lua require'dap'.run_last()<cr>", "Quit" },
		},
	}, { prefix = "<leader>" })
else
	vim.cmd([[
    nnoremap <silent> <leader>xt :lua require'dap'.toggle_breakpoint()<CR>
    nnoremap <silent> <leader>xT :lua require'dap'.set_breakpoint(vim.fn.input('Breakpoint condition: '))<CR>
    nnoremap <silent> <leader>xc :lua require'dap'.continue()<CR>
    nnoremap <silent> <leader>xb :lua require'dap'.step_back()<CR>
    nnoremap <silent> <leader>xi :lua require'dap'.step_into()<CR>
    nnoremap <silent> <leader>xo :lua require'dap'.step_over()<CR>
    nnoremap <silent> <leader>xu :lua require'dap'.step_out()<CR>
    nnoremap <silent> <leader>xC :lua require'dap'.run_to_cursor()<CR>
    nnoremap <silent> <leader>xd :lua require'dap'.disconnect()<CR>
    nnoremap <silent> <leader>xg :lua require'dap'.session()<CR>
    nnoremap <silent> <leader>xp :lua require'dap'.pause.toggle()<CR>
    nnoremap <silent> <leader>xr :lua require'dap'.repl.open()<CR>
    nnoremap <silent> <leader>xq :lua require'dap'.close()<CR>
    nnoremap <silent> <leader>xl :lua require'dap'.run_last()<CR>
    ]])
end

local dapui_ok_status, dapui = pcall(require, "dapui")
if not dapui_ok_status then
	return
end

dapui.setup({
	icons = { expanded = "▾", collapsed = "▸" },
	mappings = {
		-- Use a table to apply multiple mappings
		expand = { "<CR>", "<2-LeftMouse>" },
		open = "o",
		remove = "d",
		edit = "e",
		repl = "r",
	},
	sidebar = {
		-- You can change the order of elements in the sidebar
		elements = {
			-- Provide as ID strings or tables with "id" and "size" keys
			{
				id = "scopes",
				size = 0.25, -- Can be float or integer > 1
			},
			{ id = "breakpoints", size = 0.25 },
			{ id = "stacks", size = 0.25 },
			{ id = "watches", size = 00.25 },
		},
		size = 40,
		position = "left", -- Can be "left", "right", "top", "bottom"
	},
	tray = {
		elements = { "repl" },
		size = 10,
		position = "bottom", -- Can be "left", "right", "top", "bottom"
	},
	floating = {
		max_height = nil, -- These can be integers or a float between 0 and 1.
		max_width = nil, -- Floats will be treated as percentage of your screen.
		border = "single", -- Border style. Can be "single", "double" or "rounded"
		mappings = {
			close = { "q", "<Esc>" },
		},
	},
	windows = { indent = 1 },
})

dap.listeners.after.event_initialized["dapui_config"] = function()
  dapui.open()
end
dap.listeners.before.event_terminated["dapui_config"] = function()
  dapui.close()
end
dap.listeners.before.event_exited["dapui_config"] = function()
  dapui.close()
end
