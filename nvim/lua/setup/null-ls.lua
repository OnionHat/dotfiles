if not pcall(require, "null-ls") then
	return
end
local null_ls = require("null-ls")
null_ls.setup({
	sources = {
		null_ls.builtins.formatting.stylua,
		null_ls.builtins.diagnostics.eslint,
		null_ls.builtins.formatting.black,
		null_ls.builtins.formatting.reorder_python_imports,
		null_ls.builtins.diagnostics.mypy,
        -- null_ls.builtins.diagnostics.pylama
        -- null_ls.builtins.diagnostics.flake8,
        -- null_ls.builtins.formatting.yapf
        null_ls.builtins.formatting.shfmt
	},
})
