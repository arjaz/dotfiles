local wezterm = require 'wezterm'

local config = wezterm.config_builder()

if wezterm.gui.get_appearance():find "Dark" then
   config.color_scheme = 'Tomorrow Night'
else
   config.color_scheme = 'Modus-Operandi-Tinted'
end

config.window_padding = {
  left = 2,
  right = 2,
  top = 2,
  bottom = 2,
}

config.font = wezterm.font("Iosevka")
config.font_size = 12.0

config.hide_tab_bar_if_only_one_tab = true

return config
