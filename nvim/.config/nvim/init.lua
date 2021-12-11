vim.g.despacio_Pitch = 1
vim.g.tokyonight_style = "night"
vim.g.hybrid_reduced_contrast = 1
vim.g.hybrid_custom_term_colors = 1

vim.cmd 'highlight Normal guibg=none'

vim.cmd 'colorscheme gruberdarker'
local Reload = {}

Reload.reload_module = function(module_name, starts_with_only)
  -- TODO: Might need to handle cpath / compiled lua packages? Not sure.
  local matcher
  if not starts_with_only then
    matcher = function(pack)
      return string.find(pack, module_name, 1, true)
    end
  else
    matcher = function(pack)
      return string.find(pack, '^' .. module_name)
    end
  end

  for pack, _ in pairs(package.loaded) do
    if matcher(pack) then
      package.loaded[pack] = nil
    end
  end
end

Reload.reload_module('plugins')
Reload.reload_module('setting')
Reload.reload_module('keymaps')


require('plugins')
require('settings')
require('keymaps')

