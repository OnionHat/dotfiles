local cmp_status_ok, cmp = pcall(require, "cmp")
if not cmp_status_ok then
	return
end

local snip_status_ok, luasnip = pcall(require, "luasnip")
if not snip_status_ok then
	return
end

-- if not pcall(require, "lspkind") then
-- 	return
-- end

local check_backspace = function()
	local col = vim.fn.col(".") - 1
	return col == 0 or vim.fn.getline("."):sub(col, col):match("%s")
end

local kind_icons = {
	Text = "",
	Method = "m",
	Function = "",
	Constructor = "",
	Field = "",
	Variable = "",
	Class = "",
	Interface = "",
	Module = "",
	Property = "",
	Unit = "",
	Value = "",
	Enum = "",
	Keyword = "",
	Snippet = "",
	Color = "",
	File = "",
	Reference = "",
	Folder = "",
	EnumMember = "",
	Constant = "",
	Struct = "",
	Event = "",
	Operator = "",
	TypeParameter = "",
}

local lspkind = require("lspkind")
lspkind.init()

local cmp = require("cmp")
cmp.setup({
	completion = {
		-- autocomplete = false,
		completeopt = "menuone,noinsert,noselect",
	},
	mapping = {
		["<C-d>"] = cmp.mapping.scroll_docs(-4),
		["<C-f>"] = cmp.mapping.scroll_docs(4),
		["<C-Space>"] = cmp.mapping.complete(),
		["<C-e>"] = cmp.mapping.close(),
		["<CR>"] = cmp.mapping.confirm({ select = true }),
		["<Tab>"] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_next_item()
			elseif luasnip.expandable() then
				luasnip.expand()
			elseif luasnip.expand_or_jumpable() then
				luasnip.expand_or_jump()
			elseif check_backspace() then
				fallback()
			else
				fallback()
			end
		end, {
			"i",
			"s",
		}),
		["<S-Tab>"] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_prev_item()
			elseif luasnip.jumpable(-1) then
				luasnip.jump(-1)
			else
				fallback()
			end
		end, {
			"i",
			"s",
		}),
		-- ['<C-p>']       = cmp.mapping.select_prev_item(),
		-- ['<C-n>']       = cmp.mapping.select_next_item(),
	},
	sources = {
		{ name = "nvim_lua" },
		{ name = "nvim_lsp" },
		{ name = "path" },
		{ name = "luasnip" },
        { name = "cmp_nvim_lsp_signature_help"}
		-- { name = "buffer", keyword_length = 5 },
	},
	snippet = {
		expand = function(args)
			require("luasnip").lsp_expand(args.body)
		end,
	},
	formatting = {
		fields = { "kind", "abbr", "menu" },
		format = function(entry, vim_item)
			-- Kind icons
			vim_item.kind = string.format("%s", kind_icons[vim_item.kind])
			-- vim_item.kind = string.format('%s %s', kind_icons[vim_item.kind], vim_item.kind) -- This concatonates the icons with the name of the item kind
			vim_item.menu = ({
				nvim_lsp = "[LSP]",
				nvim_lua = "[api]",
				luasnip = "[snip]",
				buffer = "[buf]",
				path = "[path]",
			})[entry.source.name]
			return vim_item
		end,
	},
	-- documentation = {
	-- 	border = { "╭", "─", "╮", "│", "╯", "─", "╰", "│" },
	-- },
	-- formatting = {
	-- 	format = lspkind.cmp_format({
	-- 		with_text = true,
	-- 		menu = {
	-- 			buffer = "[buf]",
	-- 			nvim_lsp = "[LSP]",
	-- 			nvim_lua = "[api]",
	-- 			path = "[path]",
	-- 			luasnip = "[snip]",
	-- 		},
	-- 	}),
	-- },
	experimental = {
		native_menu = false,
		ghost_text = false,
	},
})

if not pcall(require, "nvim-autopairs") then
	return
end

require("nvim-autopairs").setup({})
-- require("nvim-autopairs.completion.cmp").setup{}
