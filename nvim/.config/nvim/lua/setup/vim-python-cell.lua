vim.cmd([[
let g:slime_target = 'tmux'

" fix paste issues in ipython
let g:slime_python_ipython = 1

" always send text to the top-right pane in the current tmux tab without asking
let g:slime_default_config = {
            \ 'socket_name': get(split($TMUX, ','), 0),
            \ 'target_pane': '{top-right}' }

let g:slime_dont_ask_default = 1
]])

local wk_ok_status, wk = pcall(require, "which-key")
if wk_ok_status then
	wk.register({
		x = {
			name = "Run Python",
			rs = { "<cmd>SlimeSend1 ipython --matplotlib<CR>", "Start IPython" },
			rr = { "<cmd>IPythonCellRun<CR>", "Run Script" },
			rt = { "<cmd>IPythonCellRunTime<CR>", "Time Execution" },
			rl = { "<cmd>IPythonCellClear<CR>", "Clear IPython" },
			rq = { "<cmd>IPythonCellClose<CR>", "Close IPython" },
		},
	}, { prefix = "<leader>" })
else
	vim.cmd([[
    nnoremap <Leader>rs :SlimeSend1 ipython --matplotlib<CR>

    " map <Leader>r to run script
    nnoremap <Leader>rr :IPythonCellRun<CR>

    " map <Leader>R to run script and time the execution
    nnoremap <Leader>rt :IPythonCellRunTime<CR>

    " map <Leader>l to clear IPython screen
    nnoremap <Leader>rl :IPythonCellClear<CR>

    " map <Leader>x to close all Matplotlib figure windows
    nnoremap <Leader>rq :IPythonCellClose<CR>
    ]])
end
