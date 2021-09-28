local cmp = require'cmp'
cmp.setup({
	snippet =
	{
		expand = function(args)
		vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` user.
		end,
	},
	mapping =
	{
		['<C-d>']       = cmp.mapping.scroll_docs(-4),
		['<C-f>']       = cmp.mapping.scroll_docs(4),
		['<C-Space>']   = cmp.mapping.complete(),
		['<C-e>']       = cmp.mapping.close(),
		['<CR>']        = cmp.mapping.confirm({ select = true }),
		['<C-p>']       = cmp.mapping.select_prev_item(),
		['<C-n>']       = cmp.mapping.select_next_item(),
	},
	sources =
	{
		{ name = 'nvim_lsp' },
		{ name = 'path' },
		{ name = 'tmux' },
		{ name = 'treesitter' },
		{ name = 'vsnip' },
		{ name = 'buffer' },
	}
})

