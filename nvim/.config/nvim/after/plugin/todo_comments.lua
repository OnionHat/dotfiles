if not pcall(require, "todo-comments") then
  return
end

-- TODO: teste
--
require("todo-comments").setup {
	signs = false,
	highlight = {
		before = "", -- "fg" or "bg" or empty
		keyword = "bg", -- "fg", "bg", "wide" or empty. (wide is the same as bg, but will also highlight surrounding characters)
		after = "fg", -- "fg" or "bg" or empty
		-- pattern = [[.*<(KEYWORDS)\s*:]], -- pattern or table of patterns, used for highlightng (vim regex)
		comments_only = true, -- uses treesitter to match keywords in comments only
		max_line_len = 400, -- ignore lines longer than this
		exclude = {}, -- list of file types to exclude highlighting
	},
	colors = {
		error = { "LspDiagnosticsDefaultError", "ErrorMsg", "#DC2626" },
		warning = { "LspDiagnosticsDefaultWarning", "WarningMsg", "#FBBF24" },
		info = { "LspDiagnosticsDefaultInformation", "#2563EB" },
		hint = { "LspDiagnosticsDefaultHint", "#10B981" },
		default = { "Identifier", "#7C3AED" },
	  },
}
