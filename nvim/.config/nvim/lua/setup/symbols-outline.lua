local ok_status, symbol = pcall(require, "symbols-outline")
if not ok_status then
    return
end
vim.g.symbols_outline = {
    auto_close = true,
    symbol_blacklist = {
     "Variable",
    },
    show_symbol_details = false,
}

vim.cmd[[
nnoremap <leader>s :SymbolsOutline<cr>
]]

