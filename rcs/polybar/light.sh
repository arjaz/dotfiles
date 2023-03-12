grep colors-light ~/.config/polybar/config.ini &&
sed -i 's/\[colors\]/\[colors-dark\]/'  ~/.config/polybar/config.ini &&
sed -i 's/\[colors-light\]/\[colors\]/' ~/.config/polybar/config.ini
