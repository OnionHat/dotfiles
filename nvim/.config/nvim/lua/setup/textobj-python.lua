vim.cmd([[
call textobj#user#map('python', {
      \   'class': {
      \     'select-a': '<buffer>ac',
      \     'select-i': '<buffer>ic',
      \     'move-n': '<buffer>]c',
      \     'move-p': '<buffer>[c',
      \   },
      \   'function': {
      \     'select-a': '<buffer>af',
      \     'select-i': '<buffer>if',
      \     'move-n': '<buffer>]f',
      \     'move-p': '<buffer>[f',
      \   }
      \ })
]])
