local lspkind = require "lspkind"
lspkind.init()

local cmp = require'cmp'
cmp.setup({
	completion =
	{
		-- autocomplete = false,
		completeopt = 'menuone,noinsert,noselect'
	},
	mapping =
	{
		['<C-d>']       = cmp.mapping.scroll_docs(-4),
		['<C-f>']       = cmp.mapping.scroll_docs(4),
		['<C-Space>']   = cmp.mapping.complete(),
		['<C-e>']       = cmp.mapping.close(),
		['<CR>']        = cmp.mapping.confirm({ select = true }),
		-- ['<C-p>']       = cmp.mapping.select_prev_item(),
		-- ['<C-n>']       = cmp.mapping.select_next_item(),
	},
	sources =
	{
		{ name = 'nvim_lua'},
		{ name = 'nvim_lsp' },
		{ name = 'path' },
		{ name = 'luasnip' },
		{ name = 'buffer', keyword_length = 5 },
	},
	snippet =
	{
		expand = function(args)
			require('luasnip').lsp_expand(args.body)
		end,
	},
	formatting =
	{
 		format = lspkind.cmp_format {
			with_text = true,
			menu =
			{
				buffer = "[buf]",
				nvim_lsp = "[LSP]",
				nvim_lua = "[api]",
				path = "[path]",
				luasnip = "[snip]",
			}
		}
	},
	experimental =
	{
		native_menu = false,
		ghost_text = true
	}

})

