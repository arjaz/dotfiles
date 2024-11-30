local wezterm = require 'wezterm'

local config = wezterm.config_builder()

if wezterm.gui.get_appearance():find "Dark" then
   config.color_scheme = 'Modus-Vivendi'
else
   config.color_scheme = 'Modus-Operandi'
end

config.window_padding = {
  left = 5,
  right = 5,
  top = 5,
  bottom = 5,
}

config.font = wezterm.font("Iosevka Extended")
config.font_size = 14.0

config.hide_tab_bar_if_only_one_tab = true

return config
