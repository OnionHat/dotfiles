if not pcall(require, "lsp_signature") then
	return
end
return require("lsp_signature").setup({
	doc_lines = 10,
	floating_window = false,
	toggle_key = "<m-x>",
	use_lspsaga = true,
	handler_opts = {
		border = "none", -- double, rounded, single, shadow, none
	},
	hint_prefix = "arg: ", -- Panda for parameter
})
